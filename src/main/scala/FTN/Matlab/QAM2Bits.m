function recvBits = QAM2Bits(QAMSymbols)
    demodulated = DynamicQamdemod(QAMSymbols); % (��̬)QAM��ӳ��
    save './data/demodulated' demodulated
    % deinterleaved = Deinterleave(demodulated); % �⽻֯
    deinterleaved = Interleaver(demodulated, 0); % �⽻֯
    save './data/deinterleaved' deinterleaved
    recvBits = Vitdec(deinterleaved); % ά�ر�����
    save './data/recvBits' recvBits
