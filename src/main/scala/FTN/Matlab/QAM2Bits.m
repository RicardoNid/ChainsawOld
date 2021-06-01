function recvBits = QAM2Bits(QAMSymbols)
    demodulated = DynamicQamdemod(QAMSymbols); % (动态)QAM解映射
    save './data/demodulated' demodulated
    % deinterleaved = Deinterleave(demodulated); % 解交织
    deinterleaved = Interleaver(demodulated, 0); % 解交织
    save './data/deinterleaved' deinterleaved
    recvBits = Vitdec(deinterleaved); % 维特比译码
    save './data/recvBits' recvBits
