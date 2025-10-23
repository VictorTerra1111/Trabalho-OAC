module cache_l1 #(
    parameter int LINES = 8,
    parameter int WORDS_PER_LINE = 8
)(
    input  logic         clk,
    input  logic         rst_n,         // active low reset

    input  logic         cpu_req,       // 1 = CPU issues a request (level or pulse)
    input  logic         cpu_rw,        // 0 = read, 1 = write
    input  logic [31:0]  cpu_addr,      // byte address
    inout  logic [31:0]  cpu_data,      // read/write data bus
    output logic         cpu_ready,     // 1 when response is ready (data valid or write done)

    // (optional) status from memory (not used in this simple model)
    input  logic         mem_ready_in    // can be tied high for simple RAM
);

    // derived cache/address parameters
    localparam int BYTE_OFFSET_BITS  = 2;
    localparam int WORD_OFFSET_BITS  = (WORDS_PER_LINE>1) ? $clog2(WORDS_PER_LINE) : 1;
    localparam int BLOCK_OFFSET_BITS = BYTE_OFFSET_BITS + WORD_OFFSET_BITS;
    localparam int INDEX_BITS        = (LINES>1) ? $clog2(LINES) : 1;
    localparam int TAG_BITS          = 32 - INDEX_BITS - BLOCK_OFFSET_BITS;
    localparam int FILL_IDX_BITS     = $clog2(WORDS_PER_LINE+1);

    // storage arrays
    logic [31:0] cache_data [0:LINES-1][0:WORDS_PER_LINE-1]; // [line][word_idx]
    logic [TAG_BITS-1:0] cache_tag [0:LINES-1];
    logic cache_valid [0:LINES-1];

    // address breakdown wires (for unconstrained cpu_addr usage)
    // tag: [31 : BLOCK_OFFSET+INDEX_BITS]
    // index: [BLOCK_OFFSET+INDEX_BITS-1 : BLOCK_OFFSET]
    // word offset: [BLOCK_OFFSET-1 : BYTE_OFFSET_BITS]
    // byte offset: [BYTE_OFFSET_BITS-1 : 0]
    // note: slices use localparams -> constant
    logic [TAG_BITS-1:0] addr_tag;
    logic [INDEX_BITS-1:0] addr_index;
    logic [WORD_OFFSET_BITS-1:0] addr_word_offset;
    logic [BYTE_OFFSET_BITS-1:0] addr_byte_offset;
    assign addr_tag         = cpu_addr[31 : (BLOCK_OFFSET_BITS + INDEX_BITS)];
    assign addr_index       = cpu_addr[(BLOCK_OFFSET_BITS + INDEX_BITS - 1) : BLOCK_OFFSET_BITS];
    assign addr_word_offset = cpu_addr[(BLOCK_OFFSET_BITS - 1) : BYTE_OFFSET_BITS];
    assign addr_byte_offset = cpu_addr[(BYTE_OFFSET_BITS - 1) : 0];

    // read data to CPU
    logic [31:0] cpu_data_out;
    logic cpu_drive_data; // when 1, cache drives cpu_data bus

    // internal RAM signals (to instantiate RAM_mem)
    logic        ram_ce_n;
    logic        ram_we_n;
    logic        ram_oe_n;
    logic        ram_bw;      // unused detail; set to 1 by default
    logic [31:0] ram_addr;
    tri  [31:0]  ram_data_bus;

    // internal control registers (single driver)
    logic        ram_ce_n_reg;
    logic        ram_we_n_reg;
    logic        ram_oe_n_reg;
    logic        ram_bw_reg;
    logic [31:0] ram_addr_reg;
    logic [31:0] ram_write_data_reg;

    // wire the regs to the ram signals used by RAM instance / tristate driver
    assign ram_ce_n = ram_ce_n_reg;
    assign ram_we_n = ram_we_n_reg;
    assign ram_oe_n = ram_oe_n_reg;
    assign ram_bw   = ram_bw_reg;
    assign ram_addr = ram_addr_reg;

    // Connect CPU data tri-state
    assign cpu_data = cpu_drive_data ? cpu_data_out : 32'bz;

    // instantiate RAM_mem (memory size example: 4 KB = 4096 bytes)
    RAM_mem #(.START_ADDRESS(32'h0000_0000), .MEMORY_SIZE(4096)) ram_inst (
        .ce_n(ram_ce_n),
        .we_n(ram_we_n),
        .oe_n(ram_oe_n),
        .bw(ram_bw),
        .address(ram_addr),
        .data(ram_data_bus)
    );

    // when we write to RAM, drive ram_data_bus from internal write_data_reg
    assign ram_data_bus = (ram_ce_n_reg==1'b0 && ram_we_n_reg==1'b0) ? ram_write_data_reg : 32'bz;

    typedef enum logic [2:0] {IDLE, LOOKUP, MISS_READ, FILL_WAIT, WRITE_DIRECT, WRITE_HIT_WAIT, RESPOND} state_t;
    state_t state, next_state;

    // counters for block fill
    logic [FILL_IDX_BITS-1:0] fill_word_idx; // 0..WORDS_PER_LINE
    logic [31:0] fill_base_addr;
    // Hit detection
    logic hit;

    // handshake registers for CPU request sampling
    logic cpu_req_latched;
    logic cpu_rw_latched;
    logic [31:0] cpu_addr_latched;
    logic [31:0] cpu_write_data_latched;

    // convenience latched decoded fields
    logic [TAG_BITS-1:0] latched_tag;
    logic [INDEX_BITS-1:0] latched_index;
    logic [WORD_OFFSET_BITS-1:0] latched_word_offset;
    logic [BYTE_OFFSET_BITS-1:0] latched_byte_offset;

    // Hit detection (based on latched index/tag when request active)
    assign hit = cpu_req_latched ? (cache_valid[latched_index] && (cache_tag[latched_index] == latched_tag)) : 1'b0;

    // to capture CPU write data when cpu issues write: sample from bus
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cpu_req_latched <= 1'b0;
            cpu_rw_latched  <= 1'b0;
            cpu_addr_latched <= 32'b0;
            cpu_write_data_latched <= 32'b0;
            latched_tag <= '0;
            latched_index <= '0;
            latched_word_offset <= '0;
            latched_byte_offset <= '0;
        end else begin
            // simple level-sensitive capture: when cpu_req asserted, latch address and rw and data
            if (cpu_req && !cpu_req_latched) begin
                cpu_req_latched <= 1'b1;
                cpu_rw_latched  <= cpu_rw;
                cpu_addr_latched <= cpu_addr;
                if (cpu_rw) cpu_write_data_latched <= cpu_data;
                // decode and store convenient fields
                latched_tag <= cpu_addr[31 : (BLOCK_OFFSET_BITS + INDEX_BITS)];
                latched_index <= cpu_addr[(BLOCK_OFFSET_BITS + INDEX_BITS - 1) : BLOCK_OFFSET_BITS];
                latched_word_offset <= cpu_addr[(BLOCK_OFFSET_BITS - 1) : BYTE_OFFSET_BITS];
                latched_byte_offset <= cpu_addr[(BYTE_OFFSET_BITS - 1) : 0];
            end
            // clear when responded
            if (state == RESPOND && !cpu_req) begin
                cpu_req_latched <= 1'b0;
            end
        end
    end

    // next-state logic (combinational) - do NOT drive RAM signals here
    always_comb begin
        next_state = state;
        cpu_drive_data = 1'b0;
        cpu_data_out = 32'b0;
        cpu_ready = 1'b0;

        case (state)
            IDLE: begin
                if (cpu_req_latched) next_state = LOOKUP;
            end

            LOOKUP: begin
                if (!cpu_rw_latched) begin
                    // READ
                    if (hit) begin
                        next_state = RESPOND;
                    end else begin
                        next_state = MISS_READ;
                    end
                end else begin
                    // WRITE
                    if (hit) begin
                        next_state = WRITE_HIT_WAIT;
                    end else begin
                        next_state = WRITE_DIRECT;
                    end
                end
            end

            MISS_READ: begin
                next_state = FILL_WAIT;
            end

            FILL_WAIT: begin
                if (fill_word_idx == WORDS_PER_LINE) begin
                    next_state = RESPOND;
                end else begin
                    next_state = FILL_WAIT;
                end
            end

            WRITE_DIRECT: begin
                next_state = RESPOND;
            end

            WRITE_HIT_WAIT: begin
                next_state = RESPOND;
            end

            RESPOND: begin
                cpu_ready = 1'b1;
                next_state = IDLE;
            end

            default: next_state = IDLE;
        endcase
    end

    // sequential logic: perform actions per state and drive RAM control regs (single driver)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= IDLE;
            fill_word_idx <= '0;
            fill_base_addr <= 32'b0;
            // invalidate cache
            for (int i=0; i<LINES; i++) begin
                cache_valid[i] <= 1'b0;
                cache_tag[i] <= {TAG_BITS{1'b0}};
                for (int j=0; j<WORDS_PER_LINE; j++) cache_data[i][j] <= 32'b0;
            end
            // default RAM inactive
            ram_ce_n_reg <= 1'b1;
            ram_we_n_reg <= 1'b1;
            ram_oe_n_reg <= 1'b1;
            ram_bw_reg   <= 1'b1;
            ram_addr_reg <= 32'b0;
            ram_write_data_reg <= 32'b0;
            cpu_drive_data <= 1'b0;
            cpu_data_out <= 32'b0;
            cpu_ready <= 1'b0;
        end else begin
            state <= next_state;

            // default RAM inactive each cycle unless a state activates it
            ram_ce_n_reg <= 1'b1;
            ram_we_n_reg <= 1'b1;
            ram_oe_n_reg <= 1'b1;
            ram_bw_reg   <= 1'b1;
            ram_addr_reg <= 32'b0;
            ram_write_data_reg <= 32'b0;
            cpu_drive_data <= 1'b0;
            cpu_data_out <= 32'b0;
            cpu_ready <= 1'b0;

            case (next_state)
                IDLE: begin
                    // nothing
                end

                LOOKUP: begin
                    // nothing synchronous here
                end

                MISS_READ: begin
                    // prepare to read block from RAM
                    // compute base address = align cpu_addr_latched to block start (zero lower BLOCK_OFFSET_BITS)
                    fill_base_addr <= { cpu_addr_latched[31 : BLOCK_OFFSET_BITS], {BLOCK_OFFSET_BITS{1'b0}} };
                    fill_word_idx <= '0;
                end

                FILL_WAIT: begin
                    // issue one RAM read (word) per cycle
                    ram_ce_n_reg <= 1'b0;
                    ram_oe_n_reg <= 1'b0;
                    ram_we_n_reg <= 1'b1;
                    ram_bw_reg   <= 1'b1;
                    // address for this word (byte address)
                    ram_addr_reg <= fill_base_addr + (fill_word_idx << BYTE_OFFSET_BITS);
                    // sample read data (ram_data_bus is combinational from RAM instance)
                    // store into cache at latched index
                    cache_data[latched_index][fill_word_idx] <= ram_data_bus;
                    // advance
                    fill_word_idx <= fill_word_idx + 1'b1;
                    // when we've just written the last word, update tag/valid
                    if (fill_word_idx == (WORDS_PER_LINE - 1)) begin
                        cache_tag[latched_index] <= latched_tag;
                        cache_valid[latched_index] <= 1'b1;
                    end
                end

                WRITE_DIRECT: begin
                    // write to RAM only (no allocate)
                    ram_ce_n_reg <= 1'b0;
                    ram_we_n_reg <= 1'b0; // active low -> write
                    ram_oe_n_reg <= 1'b1;
                    ram_addr_reg <= cpu_addr_latched;
                    ram_write_data_reg <= cpu_write_data_latched;
                    // respond next cycle
                    cpu_ready <= 1'b1;
                end

                WRITE_HIT_WAIT: begin
                    // update cache and write-through to RAM
                    cache_data[latched_index][latched_word_offset] <= cpu_write_data_latched;
                    ram_ce_n_reg <= 1'b0;
                    ram_we_n_reg <= 1'b0;
                    ram_oe_n_reg <= 1'b1;
                    ram_addr_reg <= cpu_addr_latched;
                    ram_write_data_reg <= cpu_write_data_latched;
                    cpu_ready <= 1'b1;
                end

                RESPOND: begin
                    // prepare cpu_data_out if it was a read
                    if (!cpu_rw_latched) begin
                        cpu_data_out <= cache_data[latched_index][latched_word_offset];
                        cpu_drive_data <= 1'b1;
                        cpu_ready <= 1'b1;
                    end else begin
                        cpu_ready <= 1'b1;
                    end
                    // clear latched request to accept next
                    cpu_req_latched <= 1'b0;
                end

                default: ;
            endcase
        end
    end

endmodule
```// filepath: /home/be/Work/Repositorios_Git/Trabalho-OAC/L1.sv
module cache_l1 #(
    parameter int LINES = 8,
    parameter int WORDS_PER_LINE = 8
)(
    input  logic         clk,
    input  logic         rst_n,         // active low reset

    input  logic         cpu_req,       // 1 = CPU issues a request (level or pulse)
    input  logic         cpu_rw,        // 0 = read, 1 = write
    input  logic [31:0]  cpu_addr,      // byte address
    inout  logic [31:0]  cpu_data,      // read/write data bus
    output logic         cpu_ready,     // 1 when response is ready (data valid or write done)

    // (optional) status from memory (not used in this simple model)
    input  logic         mem_ready_in    // can be tied high for simple RAM
);

    // derived cache/address parameters
    localparam int BYTE_OFFSET_BITS  = 2; // bytes per word = 4
    localparam int WORD_OFFSET_BITS  = (WORDS_PER_LINE>1) ? $clog2(WORDS_PER_LINE) : 1;
    localparam int BLOCK_OFFSET_BITS = BYTE_OFFSET_BITS + WORD_OFFSET_BITS;
    localparam int INDEX_BITS        = (LINES>1) ? $clog2(LINES) : 1;
    localparam int TAG_BITS          = 32 - INDEX_BITS - BLOCK_OFFSET_BITS;
    localparam int FILL_IDX_BITS     = $clog2(WORDS_PER_LINE+1); // to count up to WORDS_PER_LINE

    // storage arrays
    logic [31:0] cache_data [0:LINES-1][0:WORDS_PER_LINE-1]; // [line][word_idx]
    logic [TAG_BITS-1:0] cache_tag [0:LINES-1];
    logic cache_valid [0:LINES-1];

    // address breakdown wires (for unconstrained cpu_addr usage)
    // tag: [31 : BLOCK_OFFSET+INDEX_BITS]
    // index: [BLOCK_OFFSET+INDEX_BITS-1 : BLOCK_OFFSET]
    // word offset: [BLOCK_OFFSET-1 : BYTE_OFFSET_BITS]
    // byte offset: [BYTE_OFFSET_BITS-1 : 0]
    // note: slices use localparams -> constant
    logic [TAG_BITS-1:0] addr_tag;
    logic [INDEX_BITS-1:0] addr_index;
    logic [WORD_OFFSET_BITS-1:0] addr_word_offset;
    logic [BYTE_OFFSET_BITS-1:0] addr_byte_offset;
    assign addr_tag         = cpu_addr[31 : (BLOCK_OFFSET_BITS + INDEX_BITS)];
    assign addr_index       = cpu_addr[(BLOCK_OFFSET_BITS + INDEX_BITS - 1) : BLOCK_OFFSET_BITS];
    assign addr_word_offset = cpu_addr[(BLOCK_OFFSET_BITS - 1) : BYTE_OFFSET_BITS];
    assign addr_byte_offset = cpu_addr[(BYTE_OFFSET_BITS - 1) : 0];

    // read data to CPU
    logic [31:0] cpu_data_out;
    logic cpu_drive_data; // when 1, cache drives cpu_data bus

    // internal RAM signals (to instantiate RAM_mem)
    // create registers and single-driver interface
    logic        ram_ce_n;
    logic        ram_we_n;
    logic        ram_oe_n;
    logic        ram_bw;      // unused detail; set to 1 by default
    logic [31:0] ram_addr;
    tri  [31:0]  ram_data_bus;

    // internal control registers (single driver)
    logic        ram_ce_n_reg;
    logic        ram_we_n_reg;
    logic        ram_oe_n_reg;
    logic        ram_bw_reg;
    logic [31:0] ram_addr_reg;
    logic [31:0] ram_write_data_reg;

    // wire the regs to the ram signals used by RAM instance / tristate driver
    assign ram_ce_n = ram_ce_n_reg;
    assign ram_we_n = ram_we_n_reg;
    assign ram_oe_n = ram_oe_n_reg;
    assign ram_bw   = ram_bw_reg;
    assign ram_addr = ram_addr_reg;

    // Connect CPU data tri-state
    assign cpu_data = cpu_drive_data ? cpu_data_out : 32'bz;

    // instantiate RAM_mem (memory size example: 4 KB = 4096 bytes)
    RAM_mem #(.START_ADDRESS(32'h0000_0000), .MEMORY_SIZE(4096)) ram_inst (
        .ce_n(ram_ce_n),
        .we_n(ram_we_n),
        .oe_n(ram_oe_n),
        .bw(ram_bw),
        .address(ram_addr),
        .data(ram_data_bus)
    );

    // when we write to RAM, drive ram_data_bus from internal write_data_reg
    assign ram_data_bus = (ram_ce_n_reg==1'b0 && ram_we_n_reg==1'b0) ? ram_write_data_reg : 32'bz;

    // FSM states
    typedef enum logic [2:0] {IDLE, LOOKUP, MISS_READ, FILL_WAIT, WRITE_DIRECT, WRITE_HIT_WAIT, RESPOND} state_t;
    state_t state, next_state;

    // counters for block fill
    logic [FILL_IDX_BITS-1:0] fill_word_idx; // 0..WORDS_PER_LINE
    logic [31:0] fill_base_addr;
    // Hit detection
    logic hit;

    // handshake registers for CPU request sampling
    logic cpu_req_latched;
    logic cpu_rw_latched;
    logic [31:0] cpu_addr_latched;
    logic [31:0] cpu_write_data_latched;

    // convenience latched decoded fields
    logic [TAG_BITS-1:0] latched_tag;
    logic [INDEX_BITS-1:0] latched_index;
    logic [WORD_OFFSET_BITS-1:0] latched_word_offset;
    logic [BYTE_OFFSET_BITS-1:0] latched_byte_offset;

    // Hit detection (based on latched index/tag when request active)
    assign hit = cpu_req_latched ? (cache_valid[latched_index] && (cache_tag[latched_index] == latched_tag)) : 1'b0;

    // to capture CPU write data when cpu issues write: sample from bus
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cpu_req_latched <= 1'b0;
            cpu_rw_latched  <= 1'b0;
            cpu_addr_latched <= 32'b0;
            cpu_write_data_latched <= 32'b0;
            latched_tag <= '0;
            latched_index <= '0;
            latched_word_offset <= '0;
            latched_byte_offset <= '0;
        end else begin
            // simple level-sensitive capture: when cpu_req asserted, latch address and rw and data
            if (cpu_req && !cpu_req_latched) begin
                cpu_req_latched <= 1'b1;
                cpu_rw_latched  <= cpu_rw;
                cpu_addr_latched <= cpu_addr;
                if (cpu_rw) cpu_write_data_latched <= cpu_data;
                // decode and store convenient fields
                latched_tag <= cpu_addr[31 : (BLOCK_OFFSET_BITS + INDEX_BITS)];
                latched_index <= cpu_addr[(BLOCK_OFFSET_BITS + INDEX_BITS - 1) : BLOCK_OFFSET_BITS];
                latched_word_offset <= cpu_addr[(BLOCK_OFFSET_BITS - 1) : BYTE_OFFSET_BITS];
                latched_byte_offset <= cpu_addr[(BYTE_OFFSET_BITS - 1) : 0];
            end
            // clear when responded
            if (state == RESPOND && !cpu_req) begin
                cpu_req_latched <= 1'b0;
            end
        end
    end

    // next-state logic (combinational) - do NOT drive RAM signals here
    always_comb begin
        next_state = state;
        cpu_drive_data = 1'b0;
        cpu_data_out = 32'b0;
        cpu_ready = 1'b0;

        case (state)
            IDLE: begin
                if (cpu_req_latched) next_state = LOOKUP;
            end

            LOOKUP: begin
                if (!cpu_rw_latched) begin
                    // READ
                    if (hit) begin
                        next_state = RESPOND;
                    end else begin
                        next_state = MISS_READ;
                    end
                end else begin
                    // WRITE
                    if (hit) begin
                        next_state = WRITE_HIT_WAIT;
                    end else begin
                        next_state = WRITE_DIRECT;
                    end
                end
            end

            MISS_READ: begin
                next_state = FILL_WAIT;
            end

            FILL_WAIT: begin
                if (fill_word_idx == WORDS_PER_LINE) begin
                    next_state = RESPOND;
                end else begin
                    next_state = FILL_WAIT;
                end
            end

            WRITE_DIRECT: begin
                next_state = RESPOND;
            end

            WRITE_HIT_WAIT: begin
                next_state = RESPOND;
            end

            RESPOND: begin
                cpu_ready = 1'b1;
                next_state = IDLE;
            end

            default: next_state = IDLE;
        endcase
    end

    // sequential logic: perform actions per state and drive RAM control regs (single driver)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= IDLE;
            fill_word_idx <= '0;
            fill_base_addr <= 32'b0;
            // invalidate cache
            for (int i=0; i<LINES; i++) begin
                cache_valid[i] <= 1'b0;
                cache_tag[i] <= {TAG_BITS{1'b0}};
                for (int j=0; j<WORDS_PER_LINE; j++) cache_data[i][j] <= 32'b0;
            end
            // default RAM inactive
            ram_ce_n_reg <= 1'b1;
            ram_we_n_reg <= 1'b1;
            ram_oe_n_reg <= 1'b1;
            ram_bw_reg   <= 1'b1;
            ram_addr_reg <= 32'b0;
            ram_write_data_reg <= 32'b0;
            cpu_drive_data <= 1'b0;
            cpu_data_out <= 32'b0;
            cpu_ready <= 1'b0;
        end else begin
            state <= next_state;

            // default RAM inactive each cycle unless a state activates it
            ram_ce_n_reg <= 1'b1;
            ram_we_n_reg <= 1'b1;
            ram_oe_n_reg <= 1'b1;
            ram_bw_reg   <= 1'b1;
            ram_addr_reg <= 32'b0;
            ram_write_data_reg <= 32'b0;
            cpu_drive_data <= 1'b0;
            cpu_data_out <= 32'b0;
            cpu_ready <= 1'b0;

            case (next_state)
                IDLE: begin
                    // nothing
                end

                LOOKUP: begin
                    // nothing synchronous here
                end

                MISS_READ: begin
                    // prepare to read block from RAM
                    // compute base address = align cpu_addr_latched to block start (zero lower BLOCK_OFFSET_BITS)
                    fill_base_addr <= { cpu_addr_latched[31 : BLOCK_OFFSET_BITS], {BLOCK_OFFSET_BITS{1'b0}} };
                    fill_word_idx <= '0;
                end

                FILL_WAIT: begin
                    // issue one RAM read (word) per cycle
                    ram_ce_n_reg <= 1'b0;
                    ram_oe_n_reg <= 1'b0;
                    ram_we_n_reg <= 1'b1;
                    ram_bw_reg   <= 1'b1;
                    // address for this word (byte address)
                    ram_addr_reg <= fill_base_addr + (fill_word_idx << BYTE_OFFSET_BITS);
                    // sample read data (ram_data_bus is combinational from RAM instance)
                    // store into cache at latched index
                    cache_data[latched_index][fill_word_idx] <= ram_data_bus;
                    // advance
                    fill_word_idx <= fill_word_idx + 1'b1;
                    // when we've just written the last word, update tag/valid
                    if (fill_word_idx == (WORDS_PER_LINE - 1)) begin
                        cache_tag[latched_index] <= latched_tag;
                        cache_valid[latched_index] <= 1'b1;
                    end
                end

                WRITE_DIRECT: begin
                    // write to RAM only (no allocate)
                    ram_ce_n_reg <= 1'b0;
                    ram_we_n_reg <= 1'b0; // active low -> write
                    ram_oe_n_reg <= 1'b1;
                    ram_addr_reg <= cpu_addr_latched;
                    ram_write_data_reg <= cpu_write_data_latched;
                    // respond next cycle
                    cpu_ready <= 1'b1;
                end

                WRITE_HIT_WAIT: begin
                    // update cache and write-through to RAM
                    cache_data[latched_index][latched_word_offset] <= cpu_write_data_latched;
                    ram_ce_n_reg <= 1'b0;
                    ram_we_n_reg <= 1'b0;
                    ram_oe_n_reg <= 1'b1;
                    ram_addr_reg <= cpu_addr_latched;
                    ram_write_data_reg <= cpu_write_data_latched;
                    cpu_ready <= 1'b1;
                end

                RESPOND: begin
                    // prepare cpu_data_out if it was a read
                    if (!cpu_rw_latched) begin
                        cpu_data_out <= cache_data[latched_index][latched_word_offset];
                        cpu_drive_data <= 1'b1;
                        cpu_ready <= 1'b1;
                    end else begin
                        cpu_ready <= 1'b1;
                    end
                    // clear latched request to accept next
                    cpu_req_latched <= 1'b0;
                end

                default: ;
            endcase
        end
    end

endmodule