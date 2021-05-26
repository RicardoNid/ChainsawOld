function [interleavedBits] = Interleave(bits)
    global InterleaverDepth
    bits = reshape(bits, InterleaverDepth, []);
    interleavedBits = [];

    for k = 1:InterleaverDepth
        interleavedBits = [interleavedBits, bits(k, :)];
    end

    interleavedBits = interleavedBits.';
