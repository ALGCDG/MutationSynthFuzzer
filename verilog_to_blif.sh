#!bin/bash
# $1 is path to Yosys executable
$1 -p "read_verilog $2; hierarchy; proc; techmap; flatten; write_blif $3"
