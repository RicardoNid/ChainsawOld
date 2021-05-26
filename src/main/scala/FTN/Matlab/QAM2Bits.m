function bits = QAM2Bits(QAMSymbols)
    demodulated = DynamicQamdemod(QAMSymbols); % (动态)QAM解映射
    deinterleaved = Deinterleave(demodulated); % 解交织
    bits = Vitdec(deinterleaved); % 维特比译码
