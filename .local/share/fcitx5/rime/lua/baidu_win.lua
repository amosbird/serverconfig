local http = require("socket.http")
local json = require("dkjson")
local https = require("ssl.https")
local ltn12 = require("ltn12")

local code = {
    ["ca"] = "ca",
    ["cb"] = "cou",
    ["ce"] = "ce",
    ["cg"] = "ceng",
    ["cf"] = "cen",
    ["ci"] = "ci",
    ["ch"] = "cang",
    ["ck"] = "cao",
    ["cj"] = "can",
    ["cl"] = "cai",
    ["co"] = "cuo",
    ["cp"] = "cun",
    ["cs"] = "cong",
    ["cr"] = "cuan",
    ["cu"] = "cu",
    ["cv"] = "cui",
    ["ba"] = "ba",
    ["bc"] = "biao",
    ["bg"] = "beng",
    ["bf"] = "ben",
    ["bi"] = "bi",
    ["bh"] = "bang",
    ["bk"] = "bao",
    ["bj"] = "ban",
    ["bm"] = "bian",
    ["bl"] = "bai",
    ["bo"] = "bo",
    ["bn"] = "bin",
    ["bu"] = "bu",
    ["by"] = "bing",
    ["bx"] = "bie",
    ["bz"] = "bei",
    ["da"] = "da",
    ["dc"] = "diao",
    ["db"] = "dou",
    ["de"] = "de",
    ["dg"] = "deng",
    ["di"] = "di",
    ["dh"] = "dang",
    ["dk"] = "dao",
    ["dj"] = "dan",
    ["dm"] = "dian",
    ["dl"] = "dai",
    ["do"] = "duo",
    ["dq"] = "diu",
    ["dp"] = "dun",
    ["ds"] = "dong",
    ["dr"] = "duan",
    ["du"] = "du",
    ["dv"] = "dui",
    ["dy"] = "ding",
    ["dx"] = "die",
    ["dz"] = "dei",
    ["ga"] = "ga",
    ["gb"] = "gou",
    ["ge"] = "ge",
    ["gd"] = "guang",
    ["gg"] = "geng",
    ["gf"] = "gen",
    ["gh"] = "gang",
    ["gk"] = "gao",
    ["gj"] = "gan",
    ["gl"] = "gai",
    ["go"] = "guo",
    ["gp"] = "gun",
    ["gs"] = "gong",
    ["gr"] = "guan",
    ["gu"] = "gu",
    ["gw"] = "gua",
    ["gv"] = "gui",
    ["gy"] = "guai",
    ["gz"] = "gei",
    ["fa"] = "fa",
    ["fb"] = "fou",
    ["fg"] = "feng",
    ["ff"] = "fen",
    ["fh"] = "fang",
    ["fj"] = "fan",
    ["fo"] = "fo",
    ["fu"] = "fu",
    ["fz"] = "fei",
    ["ia"] = "cha",
    ["ib"] = "chou",
    ["ie"] = "che",
    ["id"] = "chuang",
    ["ig"] = "cheng",
    ["if"] = "chen",
    ["ii"] = "chi",
    ["ih"] = "chang",
    ["ik"] = "chao",
    ["ij"] = "chan",
    ["il"] = "chai",
    ["io"] = "chuo",
    ["ip"] = "chun",
    ["is"] = "chong",
    ["ir"] = "chuan",
    ["iu"] = "chu",
    ["iv"] = "chui",
    ["iy"] = "chuai",
    ["ha"] = "ha",
    ["hb"] = "hou",
    ["he"] = "he",
    ["hd"] = "huang",
    ["hg"] = "heng",
    ["hf"] = "hen",
    ["hh"] = "hang",
    ["hk"] = "hao",
    ["hj"] = "han",
    ["hl"] = "hai",
    ["ho"] = "huo",
    ["hp"] = "hun",
    ["hs"] = "hong",
    ["hr"] = "huan",
    ["hu"] = "hu",
    ["hw"] = "hua",
    ["hv"] = "hui",
    ["hy"] = "huai",
    ["hz"] = "hei",
    ["ka"] = "ka",
    ["kb"] = "kou",
    ["ke"] = "ke",
    ["kd"] = "kuang",
    ["kg"] = "keng",
    ["kf"] = "ken",
    ["kh"] = "kang",
    ["kk"] = "kao",
    ["kj"] = "kan",
    ["kl"] = "kai",
    ["ko"] = "kuo",
    ["kp"] = "kun",
    ["ks"] = "kong",
    ["kr"] = "kuan",
    ["ku"] = "ku",
    ["kw"] = "kua",
    ["kv"] = "kui",
    ["ky"] = "kuai",
    ["jc"] = "jiao",
    ["jd"] = "jiang",
    ["ji"] = "ji",
    ["jm"] = "jian",
    ["jn"] = "jin",
    ["jq"] = "jiu",
    ["jp"] = "jun",
    ["js"] = "jiong",
    ["jr"] = "juan",
    ["ju"] = "ju",
    ["jt"] = "jue",
    ["jw"] = "jia",
    ["jy"] = "jing",
    ["jx"] = "jie",
    ["ma"] = "ma",
    ["mc"] = "miao",
    ["mb"] = "mou",
    ["me"] = "me",
    ["mg"] = "meng",
    ["mf"] = "men",
    ["mi"] = "mi",
    ["mh"] = "mang",
    ["mk"] = "mao",
    ["mj"] = "man",
    ["mm"] = "mian",
    ["ml"] = "mai",
    ["mo"] = "mo",
    ["mn"] = "min",
    ["mq"] = "miu",
    ["mu"] = "mu",
    ["my"] = "ming",
    ["mx"] = "mie",
    ["mz"] = "mei",
    ["la"] = "la",
    ["lc"] = "liao",
    ["lb"] = "lou",
    ["le"] = "le",
    ["ld"] = "liang",
    ["lg"] = "leng",
    ["li"] = "li",
    ["lh"] = "lang",
    ["lk"] = "lao",
    ["lj"] = "lan",
    ["lm"] = "lian",
    ["ll"] = "lai",
    ["lo"] = "luo",
    ["ln"] = "lin",
    ["lq"] = "liu",
    ["lp"] = "lun",
    ["ls"] = "long",
    ["lr"] = "luan",
    ["lu"] = "lu",
    ["lt"] = "lve",
    ["lv"] = "lv",
    ["ly"] = "ling",
    ["lx"] = "lie",
    ["lz"] = "lei",
    ["na"] = "na",
    ["nc"] = "niao",
    ["nb"] = "nou",
    ["ne"] = "ne",
    ["nd"] = "niang",
    ["ng"] = "neng",
    ["nf"] = "nen",
    ["ni"] = "ni",
    ["nh"] = "nang",
    ["nk"] = "nao",
    ["nj"] = "nan",
    ["nm"] = "nian",
    ["nl"] = "nai",
    ["no"] = "nuo",
    ["nn"] = "nin",
    ["nq"] = "niu",
    ["ns"] = "nong",
    ["nr"] = "nuan",
    ["nu"] = "nu",
    ["nt"] = "nve",
    ["nv"] = "nv",
    ["ny"] = "ning",
    ["nx"] = "nie",
    ["nz"] = "nei",
    ["qc"] = "qiao",
    ["qd"] = "qiang",
    ["qi"] = "qi",
    ["qm"] = "qian",
    ["qn"] = "qin",
    ["qq"] = "qiu",
    ["qp"] = "qun",
    ["qs"] = "qiong",
    ["qr"] = "quan",
    ["qu"] = "qu",
    ["qt"] = "que",
    ["qw"] = "qia",
    ["qy"] = "qing",
    ["qx"] = "qie",
    ["pa"] = "pa",
    ["pc"] = "piao",
    ["pb"] = "pou",
    ["pg"] = "peng",
    ["pf"] = "pen",
    ["pi"] = "pi",
    ["ph"] = "pang",
    ["pk"] = "pao",
    ["pj"] = "pan",
    ["pm"] = "pian",
    ["pl"] = "pai",
    ["po"] = "po",
    ["pn"] = "pin",
    ["pu"] = "pu",
    ["py"] = "ping",
    ["px"] = "pie",
    ["pz"] = "pei",
    ["sa"] = "sa",
    ["sb"] = "sou",
    ["se"] = "se",
    ["sg"] = "seng",
    ["sf"] = "sen",
    ["si"] = "si",
    ["sh"] = "sang",
    ["sk"] = "sao",
    ["sj"] = "san",
    ["sl"] = "sai",
    ["so"] = "suo",
    ["sp"] = "sun",
    ["ss"] = "song",
    ["sr"] = "suan",
    ["su"] = "su",
    ["sv"] = "sui",
    ["rb"] = "rou",
    ["re"] = "re",
    ["rg"] = "reng",
    ["rf"] = "ren",
    ["ri"] = "ri",
    ["rh"] = "rang",
    ["rk"] = "rao",
    ["rj"] = "ran",
    ["ro"] = "ruo",
    ["rp"] = "run",
    ["rs"] = "rong",
    ["rr"] = "ruan",
    ["ru"] = "ru",
    ["rv"] = "rui",
    ["ua"] = "sha",
    ["ub"] = "shou",
    ["ue"] = "she",
    ["ud"] = "shuang",
    ["ug"] = "sheng",
    ["uf"] = "shen",
    ["ui"] = "shi",
    ["uh"] = "shang",
    ["uk"] = "shao",
    ["uj"] = "shan",
    ["ul"] = "shai",
    ["uo"] = "shuo",
    ["up"] = "shun",
    ["ur"] = "shuan",
    ["uu"] = "shu",
    ["uw"] = "shua",
    ["uv"] = "shui",
    ["uy"] = "shuai",
    ["uz"] = "shei",
    ["ta"] = "ta",
    ["tc"] = "tiao",
    ["tb"] = "tou",
    ["te"] = "te",
    ["tg"] = "teng",
    ["ti"] = "ti",
    ["th"] = "tang",
    ["tk"] = "tao",
    ["tj"] = "tan",
    ["tm"] = "tian",
    ["tl"] = "tai",
    ["to"] = "tuo",
    ["tp"] = "tun",
    ["ts"] = "tong",
    ["tr"] = "tuan",
    ["tu"] = "tu",
    ["tv"] = "tui",
    ["ty"] = "ting",
    ["tx"] = "tie",
    ["wa"] = "wa",
    ["wg"] = "weng",
    ["wf"] = "wen",
    ["wh"] = "wang",
    ["wj"] = "wan",
    ["wl"] = "wai",
    ["wo"] = "wo",
    ["wu"] = "wu",
    ["wz"] = "wei",
    ["va"] = "zha",
    ["vb"] = "zhou",
    ["ve"] = "zhe",
    ["vd"] = "zhuang",
    ["vg"] = "zheng",
    ["vf"] = "zhen",
    ["vi"] = "zhi",
    ["vh"] = "zhang",
    ["vk"] = "zhao",
    ["vj"] = "zhan",
    ["vl"] = "zhai",
    ["vo"] = "zhuo",
    ["vp"] = "zhun",
    ["vs"] = "zhong",
    ["vr"] = "zhuan",
    ["vu"] = "zhu",
    ["vw"] = "zhua",
    ["vv"] = "zhui",
    ["vy"] = "zhuai",
    ["ya"] = "ya",
    ["yb"] = "you",
    ["ye"] = "ye",
    ["yi"] = "yi",
    ["yh"] = "yang",
    ["yk"] = "yao",
    ["yj"] = "yan",
    ["yl"] = "yai",
    ["yo"] = "yo",
    ["yn"] = "yin",
    ["yp"] = "yun",
    ["ys"] = "yong",
    ["yr"] = "yuan",
    ["yu"] = "yu",
    ["yt"] = "yue",
    ["yy"] = "ying",
    ["xc"] = "xiao",
    ["xd"] = "xiang",
    ["xi"] = "xi",
    ["xm"] = "xian",
    ["xn"] = "xin",
    ["xq"] = "xiu",
    ["xp"] = "xun",
    ["xs"] = "xiong",
    ["xr"] = "xuan",
    ["xu"] = "xu",
    ["xt"] = "xue",
    ["xw"] = "xia",
    ["xy"] = "xing",
    ["xx"] = "xie",
    ["za"] = "za",
    ["zb"] = "zou",
    ["ze"] = "ze",
    ["zg"] = "zeng",
    ["zf"] = "zen",
    ["zi"] = "zi",
    ["zh"] = "zang",
    ["zk"] = "zao",
    ["zj"] = "zan",
    ["zl"] = "zai",
    ["zo"] = "zuo",
    ["zp"] = "zun",
    ["zs"] = "zong",
    ["zr"] = "zuan",
    ["zu"] = "zu",
    ["zv"] = "zui",
    ["zz"] = "zei",
    ["aa"] = "a",
    ["ai"] = "ai",
    ["an"] = "an",
    ["ah"] = "ang",
    ["ao"] = "ao",
    ["ee"] = "e",
    ["ei"] = "ei",
    ["en"] = "en",
    ["er"] = "er",
    ["oo"] = "o",
    ["ou"] = "ou",
    ["v"] = "zh",
    ["i"] = "ch",
    ["u"] = "sh"
}

local function url_encode(str)
    if str then
        str = string.gsub(str, "\n", "\r\n")
        str = string.gsub(str, "([^%w %-%_%.%~])",
            function(c)
                return string.format("%%%02X", string.byte(c))
            end)
        str = string.gsub(str, " ", "+")
    end
    return str
end

local function get_chinese_chars(str, len)
    local result = ""
    local count = 0
    local i = 1

    while i <= #str do
        local byte = string.byte(str, i)
        if byte >= 0xE0 and byte <= 0xEF then
            count = count + 1
            result = result .. string.sub(str, i, i + 2)
            i = i + 3
        else
            i = i + 1
        end

        if count == len then
            break
        end
    end

    return count == len and result or nil
end

local function get_baidu_suggestions(input, len)
    local url = string.format("https://www.baidu.com/sugrec?prod=pc&wd=%s", url_encode(input))
    local response_body = {}
    local res, status, headers, status_line = https.request{
        url = url,
        sink = ltn12.sink.table(response_body)
    }

    if status ~= 200 then
        log.error(status)
        return {}
    end

    local response = table.concat(response_body)
    local data, pos, err = json.decode(response)
    if err then
        log.error(tostring(err))
        return {}
    end

    local seen = {}
    local results = {}
    if data and data.g then
        for _, item in ipairs(data.g) do
            log.error(item.q)
            local chars = get_chinese_chars(item.q, len)
            if chars and not seen[chars] then
                log.error(chars)
                seen[chars] = true
                table.insert(results, chars)
            end
        end
    end

    return results
end


local flag = false

local function processor(key, env)
    local kAccepted = 1
    local kNoop = 2
    local engine = env.engine
    local context = engine.context

    if key:repr() == "Control+space" then
        return kAccepted
    elseif key:repr() == "Control+Release+space" then
        if context:is_composing() then
            flag = true
            context:refresh_non_confirmed_composition()
        end
        return kAccepted
    end
    return kNoop
end

local translator = {}

local function memoryCallback(memory, commit)
    for i, e in ipairs(commit:get()) do
        if e.comment == "+" then
            memory:update_userdict(e, 1, "")
        end
    end
    return true
end

function translator.init(env)
    env.pydb = ReverseDb("build/luna_pinyin.extended.reverse.bin")
    env.mem = Memory(env.engine, Schema("double_pinyin"))
    env.mem:memorize(function(commit) memoryCallback(env.mem, commit) end)
end

function translator.func(input, seg, env)
    if flag then
        flag = false

        local input2 = ""
        local dictstr = ""
        local len = 0
        for i = 1, #input, 2 do
            local w = input:sub(i, i + 1)
            local w2 = code[w]
            if w2 then
                input2 = input2 .. w2
                dictstr = dictstr .. w2 .. " "
            else
                input2 = input2 .. w
                dictstr = dictstr .. w .. " "
            end
            len = len + 1
        end
        local suggestions = get_baidu_suggestions(input2, len)

        local lp = ""
        local dictstr2 = ""
        if (#input % 2) == 1 then
            local trimmed = dictstr:sub(1, -2)
            local last_space = trimmed:match(".*() ")
            if last_space then
                dictstr2 = trimmed:sub(1, last_space)
                lp = trimmed:sub(last_space + 1)
            else
                lp = trimmed
            end
        end

        for _, word in ipairs(suggestions) do
            dict = DictEntry()
            dict.text = word

            if lp ~= "" then
                local pos = utf8.offset(word, -1)
                if pos then
                    local last_char = word:sub(pos)
                    local py = env.pydb:lookup(last_char)
                    local result = nil
                    for token in py:gmatch("%S+") do
                        if token:sub(1, #lp) == lp then
                            dictstr = dictstr2 .. token .. " "
                            break
                        end
                    end
                end
            end

            dict.custom_code = dictstr
            dict.comment = '+'

            local ph = Phrase(env.mem, "baidu", seg.start, seg.start + #input, dict)
            yield(ph:toCandidate())
        end
    end
end

return {processor = processor, translator = translator}
