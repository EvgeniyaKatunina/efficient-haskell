
garbage_off = false

dofile"bitmap.lua"
dofile"wavefile.lua"

local cos = math.cos
local sin = math.sin
local floor = math.floor
local sqrt = math.sqrt
local atan2 = math.atan2
local max = math.max
local min = math.min
local abs = math.abs
local pi = math.pi
local fmod = math.mod

local insert = table.insert
local remove = table.remove

local ffi = require"ffi"

icost = icost or {}
isint = isint or {}

function make_itrig(len)
	if icost[len] then return end
	icost[len] = {}
	isint[len] = {}
	for i = 0, len do
		for j = 0, len do
			icost[len][i*j] = cos(-2 * pi * i * j / len)
			isint[len][i*j] = sin(-2 * pi * i * j / len)
		end
	end
end

local function ifcos(i, j, len)
	return icost[len][i * j]
end

local function ifsin(i, j, len)
	return isint[len][i * j]
end

fwndt = {}

local function make_fwnd(len)
	if fwndt[len] then return end
	fwndt[len] = {}
	
	local log = math.log
	local sqrt = math.sqrt
	local exp = math.exp
	local cos = math.cos
	local acos = math.acos
	local atan2 = atan2
	local mod = math.mod
	local pi = math.pi
	
	local function ach(x)
		return log(x + sqrt(x*x - 1))
	end
	
	local function ch(x)
		return (exp(x) + exp(-x)) / 2
	end
	
	local function cheb_poly(n, x)
		if x >= -1 and x <= 1 then
			return cos(n * acos(x))
		else
			return ch(n * ach(x))
		end
	end
	
	local function cheby_win(out, N, atten)
		local nn
		local M, n, sum, maxy = 0, 0, 0, 0
		local tg = 10^(atten/20)
		local x0 = ch((1.0/(N-1))*ach(tg))
		M = (N-1)/2
		M = M + 0.5
		nn = 0
		while nn<(N/2+1) do
			n = nn-M;
			sum = 0
			for i=1, M do
				sum = sum + cheb_poly(N-1, x0*cos(pi*i/N))*cos(2*pi*n* i/ N)
			end
			out[nn] = tg + 2*sum
			out[N-nn-1] = out[nn]
			if(out[nn]>maxy) then
				maxy=out[nn]
			end
			nn = nn + 1
		end
		out[N] = 0
		for nn = 0, N do out[nn] = out[nn] / maxy end
	end
	
	cheby_win(fwndt[len], len, 50)
end

local function fwnd(i, len)
	return fwndt[len][i]
end

function dft2(arr, pos, len, step, rvr, rvi, off)
	make_itrig(len)
	make_itrig(len * step)
	make_fwnd(len * step)
	
	for i=0, (len - 1) do
		local cnr, cni = 0, 0
		for j = 0, (len - 1) do
			local coef = arr[pos + j * step] * fwnd(j * step + off, len * step)
			cnr = cnr + ifcos(i, j, len) * coef
			cni = cni + ifsin(i, j, len) * coef
		end
		rvr[i + 1] = cnr
		rvi[i + 1] = cni
	end
	
	return rvr, rvi
end

function ifft(arr, pos, len, step, str, sti, off)
	if len <= 4 then
		return dft2(arr, pos, len, step, str, sti, off)
	end
	
	local off = off or 0
	
	make_itrig(len)
	
	local rvr = str or {}
	local rvi = sti or {}
	
	ifft(arr, pos + step, len / 2, step * 2, rvr, rvi, off + step)
	for i=1, len / 2 do
		rvr[i + len / 2] = rvr[i]
		rvi[i + len / 2] = rvi[i]
	end
	
	ifft(arr, pos, len / 2, step * 2, rvr, rvi, off)
	
	for i=1, len/2 do
		local tr = rvr[i]
		local ti = rvi[i]
		
		local ivr = ifcos(i - 1, 1, len)
		local ivi = ifsin(i - 1, 1, len)
		
		local xdr = rvr[i + len / 2]
		local xdi = rvi[i + len / 2]
		
		rvr[i] = tr + ivr * xdr - ivi * xdi
		rvi[i] = ti + ivi * xdr + ivr * xdi
		
		rvr[i + len / 2] = tr - ivr * xdr + ivi * xdi
		rvi[i + len / 2] = ti - ivi * xdr - ivr * xdi
	end
	
	return rvr, rvi
end

function fft(arr, pos, len, phases, rvr, rvi)
	make_itrig(len)
	make_fwnd(len)
	local rvr, rvi = ifft(arr, pos, len, 1, rvr, rvi)
	for i=1, len/2 do
		local cr = rvr[i]
		local ci = rvi[i]
		rvr[i] = sqrt(cr * cr + ci * ci)
		if phases then
			rvi[i] = atan2(cr, ci)
		end
	end
	return rvr, rvi
end

function subspectrum(arr, window, start, endpos, wide, tall, fn, dc)
	dc = dc or 1
	local cramming_factor = (endpos - start) / wide / dc
	print("cram", cramming_factor)
	local bmp = bitmap.new(wide, tall, fn)
	bmp:setbgcolor(0, 0, 0)
	local ft, ph
	for i=start, endpos, dc do
		ft, ph = fft(arr, i, window, false, ft, ph)
		for j=1, window / 2 do
			local cpx, cpy = floor((i - start) * wide / (endpos - start)), floor((j - 1) * tall / window * 2)
			
			local cf = 0.15 / cramming_factor
			
			local mf1 = function(x) return math.log(x + 1) end
			local mf2, mf3 = mf1, mf1
			
			bmp:addToPixel(cpx, cpy, mf1(ft[j] / cf) * cf, mf2(ft[j] / cf) * cf, mf3(ft[j] / cf) * cf)
		end
	end
	for cc = 0, wide - 1 do
		local average = 1
		for cr = 0, tall - 1 do
			average = min(average, bmp:getRed(cc, cr))
		end
		for cr = 0, tall - 1 do
			local nv = bmp:getRed(cc, cr) - average
			nv = math.max(0, nv)
			nv = nv / (1 - average)
			bmp:setpixel(cc, cr, nv, nv, nv)
		end
	end
	bmp:writeall()
end

local st = os.clock()

if garbage_off then
    collectgarbage"stop"
end

local rdw = readWAV("../Toumei Elegy.wav")

subspectrum(rdw[1], 1024, rdw.freq*20 + 1, rdw.freq*30 + 1, 2200, 512, "../spectrum_lua.bmp", 25)
print(os.clock() - st)
