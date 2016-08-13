-- Interpreter for the minforth skeleton
-- check signedness!!

--
-- Simulated memory
---
mem = {}   

-- byte-wise memory
for i=1,65536 do
  mem[i] = 0
end

-- memory allocation
-- simple allocation by name
_memptr = 0
memory_map = {}
function alloc(name, space)
    memory_map[name] = _memptr
    _memptr = _memptr + space
end


--
-- Memory access
--

-- read one 32 bit word
function readword(addr)
    return mem[i] | (mem[i+1]<<8) |  (mem[i+2]<<16) |  (mem[i+3]<<24)
end

-- write a 32 bit word
function writeword(addr, val)
    mem[i,  val & 0xff
    mem[i+1] =  (val>>8) & 0xff
    mem[i+2] =  (val>>16) & 0xff
    mem[i+3] =  (val>>24) & 0xff    
    return addr+4
end

-- read one 8 bit byte
function readword(addr)
    return mem[i] 
end

-- write one 8 bit byte
function writeword(addr, val)
    mem[i] = val    
    return addr+1
end

-- write a sequence of bytes from a string
-- note that the length must be written separately!
function writestring(addr, val)
    for i = 1,string.len(val) do
        local c = string.sub(i, i)
        writebyte(addr+i-1, c.byte())        
    end
    return addr+val.len()
end

-- read a sequence of bytes from memory as a string
function readstring(addr, count)
    s = {}
    for i = 1,count do
        local c = string.char(readbyte(readbyte(addr+i-1)))
        table.insert(s, c)
    end
    return table.concat(s)
end




---
--- Memory map
---

alloc("origin", 0)
alloc("code", 0x8000)
alloc("word_buffer", 32)
alloc("word_length", 1)
alloc("srcptr", 4)
alloc("return_stack", 1024)
alloc("return_stack_top", 0)
alloc("data_stack", 1024)
alloc("data_stack_top", 0)
alloc("data", 0x8000)
alloc("pad", 128)
alloc("top", 0)

-- set pointers
rsp = memory_map["return_stack_top"]
fip = memory_map["cold_start"]
dsp = memory_map["data_stack_top"]
r0 = 0


---
--- data and return stack functions
--- 
function popdsp()
    readword(dsp)
    dsp = dsp - 4
end

function pushdsp(val)
    writeword(dsp, val)
    dsp = dsp + 4
end

function poprsp()
    readword(rsp)
    rsp = rsp - 4
end

function pushrsp(val)
    writeword(rsp, val)
    rsp = rsp + 4
end


--- 
--- defining commands
--- 


-- word format
-- link DWORD | namelen BYTE | name STRING | (aligned) CFA DWORD | DFA DWORD DWORD ...

_fnptr = memory_map["code"]
-- add a native code word
function addword(name, fn, flags)
    flags = flags or 0
    raw_words[name] = fn
    -- inject the _next
    mem[_fnptr] = function () fn(); _next(); end
    -- store the address of the word
    words[name] = _fnptr
    
    -- now create the word in the dictionary
    local here = readword(memory_map["HERE"])
    here = writeword(here, prev_link)    
    local l = string.length(name)
    here = writebyte(here, l|flags)    
    here = writestring(here, name)    
    -- CFA is pointer directly to the native code
    here = writeword(here, fnptr)
    -- write back new here pointer
    writeword(memory_map["HERE"])
    -- increment the code pointer
    _fnptr += 1
end

-- variables
function addvar(name, value) 
    alloc(name, 4)
    writeword(memory_map[name], value)
    addword(name, function() pushdsp(readword(memory_map[name])) end)
end
    

-- constants
function addconst(name, value)
    addword(name, function() pushdsp(value) end)
end

-- write a forth word into memory
function defword(name, words)    
        
    -- write the string into the word buffer    
    buffer(name)
    -- address and length
    pushdsp(memory_map["word_buffer"])    
    pushdsp(string.len(name))
    
    nativecall("CREATE")
    
    ptr = memory_map["HERE"]
    -- CFA is DOCOL
    ptr = writeword(ptr, words["DOCOL"])
        
    -- write in DFA
    for k,v in ipairs(words) do
        if type(k)=="string" then
            ptr = writeword(ptr, words[k])        
        else
            ptr = writeword(ptr, k)
        end
    end
    
    -- write back new here
    writeword(memory_map["HERE"], ptr)
    
end

-- flags
f_lenmask = 0x1f
f_hidden = 0x20
f_immed = 0x80

---
--- Built in constants and variables
---

-- constants
addconst("R0", memory_map["return_stack_top"])
addconst("DOCOL", words["DOCOL"])
addconst("PAD",  memory_map["pad"])
addconst("MEMTOP", memory_map["top"])
addconst("F_IMMED", f_immed)
addconst("F_HIDDEN", f_hidden)
addconst("F_LENMASK", f_lenmask)
addconst("1", 1)

-- variables    
addvar("STATE", 0)
addvar("HERE", memory_map["data"])
addvar("LATEST", 0)
addvar("S0", memory_map["data_stack_top"])

-- maps strings to addresses of the functions
words = {}


-- write a string into the word buffer
function buffer(s)
    writebyte(memory_map["word_length"],string.len(s))
    writestring(memory_map["word_buffer"], s)
end


function _next()    
    r0 = readword(fip)+4
    mem[r0]()
end


-- do we need _next() here?
addword("DOCOL", function () pushrsp(fip); fip = r0 + 4; end)
addword("EXIT", function () fip = poprsp() end)
addword("+",  function () pushdsp(popdsp()+popdsp());  end)
addword("-",  function () pushdsp(popdsp()-popdsp());  end)
addword("*",  function () pushdsp(popdsp()*popdsp());  end)
addword("AND",  function () pushdsp(popdsp()&popdsp());  end)
addword("OR",  function () pushdsp(popdsp()|popdsp());  end)
addword("XOR",  function () pushdsp(popdsp()^popdsp());  end)
addword("LSHIFT",  function () pushdsp(popdsp()<<popdsp());  end)
addword("RSHIFT",  function () pushdsp(popdsp()>>popdsp());  end)
addword("=",  function () if(popdsp()==popdsp()) then pushdsp(1) else pushdsp(0);  end)
addword("<",  function () if(popdsp()<popdsp()) then pushdsp(1) else pushdsp(0);  end)
addword(">",  function () if(popdsp()>popdsp()) then pushdsp(1) else pushdsp(0);  end)
addword("!",  function () addr=popdsp();  val=popdsp(); writeword(addr, val); ; end)
addword("@",  function () addr=popdsp(); pushdsp(readword(addr)); ; end)
addword("C!",  function () addr=popdsp();  val=popdsp(); mem[addr]=val&0xff; ; end)
addword("C@",  function () addr=popdsp(); pushdsp(mem[addr]); ; end)
addword(">R",  function () pushrsp(popdsp()); ; end)
addword("R>",  function () pushdsp(poprsp()); ; end)
addword("RSP@",  function () pushdsp(rsp); ; end)
addword("RSP!",  function () rsp = popdsp; ; end)
addword("RSP@",  function () pushdsp(rsp); ; end)
addword("DSP!",  function () dsp = popdsp; ; end)
addword("DSP@",  function () pushdsp(dsp); ; end)
addword("FIP!",  function () fip = popdsp; ; end)
addword("FIP@",  function () pushdsp(fip); ; end)

-- read one byte from the source
addword("MEMKEY", function() 
    ptr = readword(memory_map["srcptr"])
    readbyte(ptr)
    writeword(memory_map["srcptr"]+1)
    pushdsp(ptr)    
end


-- call a word without _next()
function nativecall(name)
    raw_words[name]()
end
        
addword("CREATE", function()
    local l = popdsp()
    local addr = popdsp()
    local latest = readword(memory_map["LATEST"])
    local here = readword(memory_map["HERE"])
    here = writeword(here, latest) -- link pointer
    -- write in name
    here = writebyte(here, l)
    for i=1,l do 
        here = writebyte(here, readbyte(addr+l-1))
    end
    -- align
    here = (here + 3) & (~3)
    
    -- write back variables
    writeword(memory_map["LATEST"], here)
    writeword(memory_map["HERE"], here)       
)
        
addword(">CFA", function()
    -- skip link, string len, string, then align to find the CFA
    local addr = popdsp()
    addr = addr + 4
    local l = readbyte(addr) & f_lenmask
    addr = (addr + 4 + l) & (~3)    
    pushdsp(addr)    
end)
        
addword("MEMWORD", function() 
    -- eat whitespace
    nativecall("MEMKEY")
    local c = popdsp()
    while c<=" " do
        nativecall("MEMKEY")
        local c = popdsp()
    local n = 0
    -- find characters
    while c>" " do
        writebyte(memory_map["word_buffer"]+n, c)
        n += 1
        nativecall("MEMKEY")
        c = popdsp()
    -- store the length
    writebyte(memory_map["word_length"], n)            
end

addword("FIND", function()
    local last_word = readword(memory_map["LATEST"])
    local l = popdsp()
    local addr = popdsp()
    local match = false
    while not found do
        -- no match
        if last_word==0 then return end
        local ptr = last_word+1
        local ldict = readbyte(ptr)
        
        if l == (ldict & (f_lenmask|f_hidden)) then
            ptr = ptr + 1
            local wptr = addr
            match = true
            for i=1,l do
                local cdict = readbyte(ptr)
                local cword = readbyte(wptr)
                if cword /= cdict then
                    match = false
                    break 
                end
                ptr = ptr + 1
            end
        end            
        -- follow the link
        last_word = readword(last_word)
    end
    if match then
        pushdsp(last_word)
    end
)

addword(",", function () 
    local here = readword(memory_map["HERE"])
    here = writeword(here, popdsp())
    writeword(memory_map["HERE"], here)
end

addword("[[", function() 
    writeword(memory_map["STATE"], 0) end)
    
addword("]]", function() 
    writeword(memory_map["STATE"], 1) end)
    
addword("'", function()
    local tick = readword(fip)
    fip += 4
    pushdsp(tick)
)    

addword("SOURCE", function()
    pushdsp(memory_map["source_start"])
    pushdsp(memory_map["source_len"])
    end
)

addword("BRANCH", function()
    fip = fip + readword(fip)    
end
)

addword("0BRANCH", function()
    local i = popdsp()
    if i==0 then
        fip = fip + readword(fip)        
    else
        -- skip the offset
        fip = fip + 4
    end    
end
)

-- to be completed...
addword("LITS", function()
    local r = fip
    fip = fip + 4
    pushdsp(fip)
    pushdsp(fip-4)
)

-- execute an instruction
addword("EXECUTE", function() mem[popdsp()](); end)

-- pre-defined Forth words
defword("QUIT", ["INTERPRET", "BRANCH", -8])
defword(";", ["'", "EXIT", ",", "[[", "EXIT"], f_immed)
defword(":", ["WORD", "CREATE", "DOCOL", ",", "]]", "EXIT"], f_immed)
defword("KEY", ["MEMKEY", "EXIT"])
defword("WORD", ["MEMWORD", "EXIT"])


defword("cold_start", ["QUIT"])

-- point fip to quit
pushdsp(memory_map["LATEST"])
nativecall(">CFA")
fip = popdsp()
    