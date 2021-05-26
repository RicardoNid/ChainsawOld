function [out, noise] = addnoise(input, attn)
    iout = randn(1, length(input)) .* attn;
    noise = iout';
    out = input + noise;
end
