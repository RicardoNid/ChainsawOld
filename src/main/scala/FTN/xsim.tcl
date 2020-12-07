start_gui
create_project FIR_IP C:/Users/lsfan/FIR_IP -part xczu7ev-ffvc1156-2-e
set_property board_part xilinx.com:zcu104:part0:1.1 [current_project]
file mkdir C:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/new
close [ open C:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/new/FIR_IP.sv w ]
add_files C:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/new/FIR_IP.sv
update_compile_order -fileset sources_1
export_ip_user_files -of_objects  [get_files C:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/new/FIR_IP.sv] -no_script -reset -force -quiet
remove_files  C:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/new/FIR_IP.sv
add_files -norecurse C:/Users/lsfan/FIR_IP/FIR_IP.srcs/FIR_IP.sv
create_ip -name fir_compiler -vendor xilinx.com -library ip -version 7.2 -module_name BASIC_FIR -dir c:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/ip
set_property -dict [list CONFIG.Component_Name {BASIC_FIR}] [get_ips BASIC_FIR]
generate_target {instantiation_template} [get_files c:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/ip/BASIC_FIR/BASIC_FIR.xci]
generate_target all [get_files  c:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/ip/BASIC_FIR/BASIC_FIR.xci]
catch { config_ip_cache -export [get_ips -all BASIC_FIR] }
export_ip_user_files -of_objects [get_files c:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/ip/BASIC_FIR/BASIC_FIR.xci] -no_script -sync -force -quiet
create_ip_run [get_files -of_objects [get_fileset sources_1] c:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/ip/BASIC_FIR/BASIC_FIR.xci]
launch_runs BASIC_FIR_synth_1 -jobs 10
export_simulation -of_objects [get_files c:/Users/lsfan/FIR_IP/FIR_IP.srcs/sources_1/ip/BASIC_FIR/BASIC_FIR.xci] -directory C:/Users/lsfan/FIR_IP/FIR_IP.ip_user_files/sim_scripts -ip_user_files_dir C:/Users/lsfan/FIR_IP/FIR_IP.ip_user_files -ipstatic_source_dir C:/Users/lsfan/FIR_IP/FIR_IP.ip_user_files/ipstatic -lib_map_path [list {modelsim=C:/Users/lsfan/FIR_IP/FIR_IP.cache/compile_simlib/modelsim} {questa=C:/Users/lsfan/FIR_IP/FIR_IP.cache/compile_simlib/questa} {riviera=C:/Users/lsfan/FIR_IP/FIR_IP.cache/compile_simlib/riviera} {activehdl=C:/Users/lsfan/FIR_IP/FIR_IP.cache/compile_simlib/activehdl}] -use_ip_compiled_libs -force -quiet
add_files -norecurse {C:/Users/lsfan/FIR_IP/FIR_IP.srcs/BASIC_FIR_TB.sv C:/Users/lsfan/FIR_IP/FIR_IP.srcs/FIRIP.sv C:/Users/lsfan/FIR_IP/FIR_IP.srcs/BASIC_FIR_WRAPPER.sv}
update_compile_order -fileset sources_1
update_compile_order -fileset sources_1
update_compile_order -fileset sources_1
export_ip_user_files -of_objects  [get_files C:/Users/lsfan/FIR_IP/FIR_IP.srcs/FIR_IP.sv] -no_script -reset -force -quiet
remove_files  C:/Users/lsfan/FIR_IP/FIR_IP.srcs/FIR_IP.sv
set_property top BASIC_FIR_WRAPPER [current_fileset]
update_compile_order -fileset sources_1
launch_simulation
source BASIC_FIR_WRAPPER_tb.tcl
run 500 ns
update_compile_order -fileset sources_1
update_compile_order -fileset sources_1
update_compile_order -fileset sources_1
update_compile_order -fileset sources_1
update_compile_order -fileset sources_1
close_sim
launch_simulation
source BASIC_FIR_WRAPPER_tb.tcl
restart
run 500 ns
run 500 ns
run 500 ns
run 500 ns
run 2000 ns
save_wave_config {C:/Users/lsfan/FIR_IP/basic.wcfg}
add_files -fileset sim_1 -norecurse C:/Users/lsfan/FIR_IP/basic.wcfg
set_property xsim.view C:/Users/lsfan/FIR_IP/basic.wcfg [get_filesets sim_1]
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
restart
run 2000 ns
restart
run 2000 ns
save_wave_config {C:/Users/lsfan/FIR_IP/basic.wcfg}
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
restart
run 2000 ns
save_wave_config {C:/Users/lsfan/FIR_IP/basic.wcfg}
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
save_wave_config {C:/Users/lsfan/FIR_IP/basic.wcfg}
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
save_wave_config {C:/Users/lsfan/FIR_IP/basic.wcfg}
close_sim
launch_simulation
open_wave_config C:/Users/lsfan/FIR_IP/basic.wcfg
source BASIC_FIR_WRAPPER_tb.tcl
