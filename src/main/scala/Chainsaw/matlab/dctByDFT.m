indices = 1:16;
data = sin(indices)

expand2n = [data, flip(data)] ./ 2;
expand4n = [zeros(size(expand2n)); expand2n];
fft4n = reshape(expand4n, [], 1);

byfft = fft(fft4n);
bydct = dct2(data);

plot(indices, bydct, 'g', indices, byfft(indices), 'm')
legend({'original', 'byfft'})

sum(data)
bydct(1)