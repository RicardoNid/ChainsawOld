function [deinterleavedBits] = Deinterleave(bits)
    global InterleaverDepth
    len = length(bits) / InterleaverDepth;
    deinterleavedBits = [];

    for k = 1:InterleaverDepth
        deinterleavedBits = [deinterleavedBits; bits(len * (k - 1) + 1:len * k)];
    end

    deinterleavedBits = deinterleavedBits(:); % ½µÎ¬
