module mp(
    input logic end_i,
    input logic ctrl_i,
    
    input logic clk100Hz,
    input logic reset, 
    
    inout logic data,
    output logic status_o
)

