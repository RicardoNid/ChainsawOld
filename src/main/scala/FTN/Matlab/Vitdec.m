% ά�ر�����
function [decodedBits] = Vitdec(bits)
    global trellis
    global tblen
    decodedBits = vitdec(bits, trellis, tblen, 'cont', 'hard'); % ά�ر�������õĲ����������ļ�
    padding = ones(tblen, 1);
    decodedBits = [decodedBits(tblen + 1:end); padding]; % ����һ�������ı���λ,��Ϊά�ر������޷��õ���ȷ��β��
