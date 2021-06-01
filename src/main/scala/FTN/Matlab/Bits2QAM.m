function QAMSymbols = Bits2QAM(bits)
    save './data/bits' bits;
    convCodedBits = Convenc(bits); % �������
    save './data/convCodedBits' convCodedBits;
    interleavedBits = Interleaver(convCodedBits, 1); % ��֯
    save './data/interleavedBits' interleavedBits;
    QAMSymbols = DynamicQammod(interleavedBits); % (��̬)QAMӳ��
    save './data/QAMSymbols' QAMSymbols;
