
local ffi = require"ffi"

ffi.cdef([[
struct px {
    double r, g, b;
};
]])

bitmap = {}

local floor = math.floor
local char = string.char
local byte = string.byte
local insert = table.insert

bitmap.new = function(width, height, fn)
	local this = {}
	
	this.width = width
	this.height = height
	this.file = io.open(fn, "wb")
	this.pixels = ffi.new("struct px["..width*height.."]")
	this.bgcolor = char(255, 255, 255)
	
	setmetatable(this, {__index = bitmap})
	this:writeheader()
	return this
end

local function dword(x)
	local s = ''
	
	for i = 1, 4 do
		s = s .. char(x % 256)
		x = floor(x/256)
	end
	
	return s
end
	
local function trunc(x)
	if x < 0 then
		x = 0
	elseif x > 1 then
		x = 1
	end
	
	return floor(255*x)
end

bitmap.writeheader = function(this)
	local f = this.file
	
	local fhdr = 14
	local ihdr = 40
	
	f:write'BM'
	f:write(dword(fhdr + ihdr + this.width*this.height*3))
	f:write'\0\0\0\0'
	f:write(dword(fhdr + ihdr))
	f:write(dword(ihdr))
	f:write(dword(this.width))
	f:write(dword(this.height))
	f:write'\1\0'
	f:write'\24\0'
	f:write'\0\0\0\0'
	f:write'\0\0\0\0'
	f:write'\0\0\0\0'
	f:write'\0\0\0\0'
	f:write'\0\0\0\0'
	f:write'\0\0\0\0'
end

local function forall(f, ...)
	local arg = {...}
	for k,v in ipairs(arg) do
		arg[k] = f(v)
	end
	return unpack(arg)
end

bitmap.setpixel = function(this, x, y, r, g, b)
    local idx = x + y * this.width
    this.pixels[idx].r = r
    this.pixels[idx].g = g
    this.pixels[idx].b = b
end

bitmap.setbgcolor = function(this, r, g, b)
    for i= 1, this.width*this.height do
        this.pixels[i - 1].r = r
        this.pixels[i - 1].g = g
        this.pixels[i - 1].b = b
    end
end

bitmap.addToPixel = function(this, x, y, r, g, b)
    local idx = x + y * this.width
    this.pixels[idx].r = this.pixels[idx].r + r
    this.pixels[idx].g = this.pixels[idx].g + g
    this.pixels[idx].b = this.pixels[idx].b + b
end

bitmap.getpixel = function(this, x, y)
	local cp = this.pixels[x + y * this.width]
	return cp
end

bitmap.getRed = function(this, x, y)
    return this.pixels[x + y * this.width].r
end

bitmap.writeall = function(this)
    local datas = {}
	for i=1, this.width * this.height do
		local cp = this.pixels[i - 1]
		insert(datas, char(forall(trunc, cp.r, cp.g, cp.b)))
	end
	this.file:write(table.concat(datas, ""))
	this.file:close()
end
