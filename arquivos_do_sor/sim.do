if {[file isdirectory work]} { vdel -all -lib work }
vlib work
vmap work work

vcom mult_div.vhd
vcom MIPS-MC_SingleEdge.vhd
vcom MIPS-MC_SingleEdge_tb.vhd

vsim -voptargs=+acc=lprn -t ps work.CPU_tb

do wave.do

set StdArithNoWarnings 1
set StdVitalGlitchNoWarnings 1

run 50 us

