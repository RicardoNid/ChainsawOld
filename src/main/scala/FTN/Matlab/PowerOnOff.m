function Symbols = PowerOnOff(Symbols)
    % 将PowerOn/Off单独写成函数,是因为它们在硬件上对应相同的独立模块
    % 考虑使用DFX对其进行实现
    global SToPcol
    global PowerOn
    load('./data/powerAlloc.mat'); % 功率分配,训练模式后接收机反馈的信息之一,power_alloc尺寸1*224

    for i = 1:SToPcol

        if PowerOn == 1
            Symbols(:, i) = Symbols(:, i) .* sqrt(powerAlloc');
        else
            Symbols(:, i) = Symbols(:, i) ./ sqrt(powerAlloc');
        end

    end
