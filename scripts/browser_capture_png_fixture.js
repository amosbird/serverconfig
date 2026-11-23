"use strict";
const assert = require("node:assert/strict");
const {inflateSync} = require("node:zlib");

// Bounded decoder for Chrome's non-interlaced RGB/RGBA screenshot fixtures only.
function decodePng(png) {
    assert.ok(png.length >= 33 && png.length <= 500000);
    assert.deepEqual(png.subarray(0, 8), Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]));
    assert.equal(png.toString("ascii", 12, 16), "IHDR");
    const width = png.readUInt32BE(16), height = png.readUInt32BE(20);
    assert.ok(width > 0 && height > 0 && width * height <= 16000000);
    assert.equal(png[24], 8);
    assert.equal(png[26], 0); assert.equal(png[27], 0); assert.equal(png[28], 0);
    const channels = {2: 3, 6: 4}[png[25]];
    assert.ok(channels, `Unsupported PNG color type ${png[25]}`);
    const chunks = [];
    let ended = false;
    for (let offset = 8; offset < png.length;) {
        assert.ok(offset + 12 <= png.length);
        const length = png.readUInt32BE(offset);
        assert.ok(length <= png.length - offset - 12);
        const type = png.toString("ascii", offset + 4, offset + 8);
        if (type === "IDAT") chunks.push(png.subarray(offset + 8, offset + 8 + length));
        offset += length + 12;
        if (type === "IEND") {
            assert.equal(length, 0); assert.equal(offset, png.length); ended = true;
        }
    }
    assert.ok(ended && chunks.length);
    const stride = width * channels;
    const raw = inflateSync(Buffer.concat(chunks), {maxOutputLength: (stride + 1) * height});
    assert.equal(raw.length, (stride + 1) * height);
    const pixels = Buffer.alloc(stride * height);
    function paeth(a, b, c) {
        const p = a + b - c, pa = Math.abs(p - a), pb = Math.abs(p - b), pc = Math.abs(p - c);
        return pa <= pb && pa <= pc ? a : pb <= pc ? b : c;
    }
    for (let y = 0; y < height; y++) {
        const filter = raw[y * (stride + 1)];
        assert.ok(filter <= 4);
        for (let x = 0; x < stride; x++) {
            const index = y * stride + x;
            const a = x >= channels ? pixels[index - channels] : 0;
            const b = y ? pixels[index - stride] : 0;
            const c = y && x >= channels ? pixels[index - stride - channels] : 0;
            pixels[index] = (raw[y * (stride + 1) + 1 + x] +
                [0, a, b, Math.floor((a + b) / 2), paeth(a, b, c)][filter]) & 255;
        }
    }
    return {width, height, pixel(x, y) {
        assert.ok(Number.isInteger(x) && x >= 0 && x < width);
        assert.ok(Number.isInteger(y) && y >= 0 && y < height);
        return [...pixels.subarray(y * stride + x * channels, y * stride + x * channels + 3)];
    }};
}
module.exports = {decodePng};
