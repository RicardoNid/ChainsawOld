close all
warning off all
clc

InitOFDMParameters(); % 初始化全局变量
PrepareROM(); % 将无需硬件计算(存储在ROM中)的数据准备好
global On
global CurrentFrame

Run(); % 以比特分配(训练)模式运行一次系统

On = 1;
CurrentFrame = 1;

Run(); % 以比特加载(工作)模式运行一次系统

%% 运行完毕之后,清除数据文件,以防残留的数据文件掩盖代码修改后的问题
% 比特分配数据
delete './data/powerAlloc.mat'
delete './data/bitAlloc.mat'
delete './data/bitAllocSort.mat'
delete './data/bitAllocSum.mat'
% ROM预存数据
delete './data/preambleQAMSymbols.mat'
delete './data/msgQAMSymbols.mat'
