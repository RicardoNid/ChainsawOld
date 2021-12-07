function ret = dct2(data)
    dctSize = length(data);
    ret = zeros(size(data));
    for k = 1:dctSize
        for i = 1:dctSize
            coeff = cos(pi * (2 * (i-1) + 1) * (k-1) / (2 * dctSize));
            ret(k) = ret(k) + data(i) * coeff;
        end
    end
end