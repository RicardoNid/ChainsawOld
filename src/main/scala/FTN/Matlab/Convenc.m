function [convCodedBits] = Convenc(bits)
    global trellis % ���������õĲ����������ļ�
    convCodedBits = convenc(bits, trellis); % �������
