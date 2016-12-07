
local char = string.char
local floor = math.floor

local function int(nes)
	return nes:byte(1) + nes:byte(2) * 256 + nes:byte(3) * 0x10000 + nes:byte(4) * 0x1000000
end

local function short(nes)
	return nes:byte(1) + nes:byte(2) * 256
end

local function sshort(nes)
	local ob = nes:byte(2)
	if ob >= 0x80 then
		return -(65535 - nes:byte(1) - ob * 256)
	else
		return nes:byte(1) + ob * 256
	end
end

function readWAV(fn)
	local f = io.open(fn, "rb")
	
	local rv = {}
	
	-- header
	local id = f:read(4)
	assert(id == "RIFF", "not a riff")
	local sz = int(f:read(4))
	
	id = f:read(4)
	print("id", id)
	id = f:read(4)
	print("id2", id)
	
	local sz2 = int(f:read(4))
	
	print(sz,sz2)
	
	local fmt = short(f:read(2))
	local chans = short(f:read(2))
	local samplerate = int(f:read(4))
	local byterate = int(f:read(4))
	local blockalign = short(f:read(2))
	local bits = short(f:read(2))
	
	print(fmt, chans, samplerate, byterate, blockalign, bits)
	
	rv.freq = samplerate
	
	assert(bits == 16, "bits != 16, u mad bro")
	
	id = f:read(4)
	print("id3", id)
	
	local sz3 = int(f:read(4))
	print("sz3", sz3)
	local data = f:read(sz3 * bits/8 * chans)
	
	f:close()
	
	for i=1, chans do
		rv[i] = {}
	end
	
	local ic = 1
	for i = 1, sz3, bits * chans / 8 do
		for j=0, chans - 1 do
			local csmp = data:sub(i + bits/8 * j, i + bits/8 * (j + 1) - 1)
			--print(short(csmp))
			rv[j + 1][ic] = (sshort(csmp)) / 65536
		end
		ic = ic + 1
	end
	
	print(#rv[1], #data / bits * 8 / chans, #rv[1] / rv.freq)
	
	return rv
end
