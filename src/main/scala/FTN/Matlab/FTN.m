close all
warning off all
clc

InitOFDMParameters(); % ��ʼ��ȫ�ֱ���
PrepareROM(); % ������Ӳ������(�洢��ROM��)������׼����
global On
global CurrentFrame

Run(); % �Ա��ط���(ѵ��)ģʽ����һ��ϵͳ

On = 1;
CurrentFrame = 1;

Run(); % �Ա��ؼ���(����)ģʽ����һ��ϵͳ

%% �������֮��,��������ļ�,�Է������������ļ��ڸǴ����޸ĺ������
% ���ط�������
delete './data/powerAlloc.mat'
delete './data/bitAlloc.mat'
delete './data/bitAllocSort.mat'
delete './data/bitAllocSum.mat'
% ROMԤ������
delete './data/preambleQAMSymbols.mat'
delete './data/msgQAMSymbols.mat'
