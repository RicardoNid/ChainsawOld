function bits = QAM2Bits(QAMSymbols)
    demodulated = DynamicQamdemod(QAMSymbols); % (��̬)QAM��ӳ��
    deinterleaved = Deinterleave(demodulated); % �⽻֯
    bits = Vitdec(deinterleaved); % ά�ر�����
