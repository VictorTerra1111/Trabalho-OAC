onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -color White /cpu_tb/ck
add wave -noupdate -color White /cpu_tb/rst


add wave -noupdate -divider sinais
add wave /cpu_tb/Dadress
add wave /cpu_tb/Ddata
add wave /cpu_tb/Iadress
add wave /cpu_tb/Idata
add wave /cpu_tb/i_cpu_address
add wave /cpu_tb/d_cpu_address
add wave /cpu_tb/data_cpu
add wave /cpu_tb/tb_add
add wave /cpu_tb/tb_data
add wave /cpu_tb/Dce_n
add wave /cpu_tb/Dwe_n
add wave /cpu_tb/Doe_n
add wave /cpu_tb/Ice_n
add wave /cpu_tb/Iwe_n
add wave /cpu_tb/Ioe_n
add wave /cpu_tb/rstCPU
add wave /cpu_tb/hold
add wave /cpu_tb/readInst
add wave /cpu_tb/cpu/uins.i
add wave /cpu_tb/go_i
add wave /cpu_tb/go_d
add wave /cpu_tb/ce
add wave /cpu_tb/rw
add wave /cpu_tb/bw