function QAMSymbols = Bits2QAM(bits)
    convCodedBits = Convenc(bits); % �������
    save './data/convCodedBits' convCodedBits;
    interleavedBits = Interleave(convCodedBits); % ��֯
    save './data/interleavedBits' interleavedBits;
    QAMSymbols = DynamicQammod(interleavedBits); % (��̬)QAMӳ��
    save './data/QAMSymbols' QAMSymbols;
