function bits = BitGen()
    global BitNumber
    global CurrentFrame
    global tblen
    global Seed
    % bits = randint(BitNumber, 1, 2, Seed(CurrentFrame));
    rng(Seed(CurrentFrame))
    bits = randi(2, BitNumber, 1) - 1;
    bits(length(bits) - tblen:length(bits)) = 1; % ����һ�������ı���λ,��Ϊά�ر������޷��õ���ȷ��β��
