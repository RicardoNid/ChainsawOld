function Symbols = PowerOnOff(Symbols)
    % ��PowerOn/Off����д�ɺ���,����Ϊ������Ӳ���϶�Ӧ��ͬ�Ķ���ģ��
    % ����ʹ��DFX�������ʵ��
    global SToPcol
    global PowerOn
    load('./data/powerAlloc.mat'); % ���ʷ���,ѵ��ģʽ����ջ���������Ϣ֮һ,power_alloc�ߴ�1*224

    for i = 1:SToPcol

        if PowerOn == 1
            Symbols(:, i) = Symbols(:, i) .* sqrt(powerAlloc');
        else
            Symbols(:, i) = Symbols(:, i) ./ sqrt(powerAlloc');
        end

    end
