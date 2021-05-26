% 维特比译码
function [decodedBits] = Vitdec(bits)
    global trellis
    global tblen
    decodedBits = vitdec(bits, trellis, tblen, 'cont', 'hard'); % 维特比译码采用的参数见参数文件
    padding = ones(tblen, 1);
    decodedBits = [decodedBits(tblen + 1:end); padding]; % 放弃一定数量的比特位,因为维特比译码无法得到正确的尾部
