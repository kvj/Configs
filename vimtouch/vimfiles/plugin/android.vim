let s:T_TypeString = 's'
let s:T_TypeNumber = 'i'
let s:T_TypeDouble = 'd'
let s:T_TypeBoolean = 'b'
let s:T_TypeList = 'l'
let s:T_TypeMap = 'm'
let s:T_TypeTransferable = 'o'
let s:T_TypeNull = 'n'

"let s:TestTransferable = '1,a,i1,1,b,i-1,1,s,s3,aaa,1,f,d1.5,4,strs,l3,s2,s1,n,s2,s3,2,tr,o8,SubClass,2,f1,i2,2,f2,i0,0,,6,intmap,m3,s2,f3,i3,s2,f2,i2,s2,f1,i1,0,,'

fu! s:T_ToString(obj)
	let obj = a:obj
	if type(0) == type(obj)
		return ''.obj
	endif
	if type("") == type(obj)
		return "'".obj."'"
	endif
	if type(0.0) == type(obj)
		return printf('%f', obj)
	endif
	if type({}) == type(obj)
		let out = '{'
		let isObject = get(obj, '-', '') != ''
		if isObject
			let out .= obj['-'].'{'
		endif
		for [key, value] in items(obj)
			if key != '-'
				let out .= ' '.key.'='.s:T_ToString(value)
			endif
			unlet value
		endfor
		let out .= ' }'
		if isObject
			let out .= '}'
		endif
		return out
	endif
	if type([]) == type(obj)
		let out = '['
		for item in obj
			let out .= ' '.s:T_ToString(item)
		endfor
		let out .= ' ]'
		return out
	endif
	return '???'
endfu

fu! s:T_NextPiece(str, index)
	let comma = stridx(a:str, ',', a:index)
	if comma == -1
		"Not found
		throw 'Out of bounds: '.a:index
	endif
	return [comma+1, strpart(a:str, a:index, comma-a:index)]
endf

fu! s:T_NextInt(str, index)
	let [idx, piece] = s:T_NextPiece(a:str, a:index)
	"echo 'T_NextInt '.idx.' '.piece
	return [idx, str2nr(piece, 10)]
endf

fu! s:T_NextStr(str, index)
	let [idx, strlen] = s:T_NextInt(a:str, a:index)
	"echo 'T_NextStr '.a:index.' -> '.idx.' '.strlen
	return[idx+strlen+1, strpart(a:str, idx, strlen)]
endf

fun! s:T_Parse_i(str, index)
	let [idx, str] = s:T_NextPiece(a:str, a:index)
	return [idx, str2nr(str), s:T_TypeNumber]
endf

fun! s:T_Parse_b(str, index)
	let [idx, str] = s:T_NextPiece(a:str, a:index)
	return [idx, str2nr(str) == 0 ? 0 : 1, s:T_TypeBoolean]
endf

fun! s:T_Parse_d(str, index)
	let [idx, str] = s:T_NextPiece(a:str, a:index)
	return [idx, str2float(str), s:T_TypeDouble]
endf

fun! s:T_Parse_l(str, index)
	let [idx, num] = s:T_NextInt(a:str, a:index)
	let result = []
	for i in range(num)
		let [idx, value, type] = s:T_ParseNext(a:str, idx)
		call add(result, value)
		unlet value
	endfor
	return [idx, result, s:T_TypeList]
endf

fun! s:T_Parse_m(str, index)
	let [idx, num] = s:T_NextInt(a:str, a:index)
	let result = {}
	for i in range(num)
		let [idx, field, type] = s:T_ParseNext(a:str, idx)
		let [idx, value, type] = s:T_ParseNext(a:str, idx)
		if type != s:T_TypeNull
			let result[field] = value
		endif
		unlet value
	endfor
	return [idx, result, s:T_TypeMap]
endf

fun! s:T_Parse_o(str, index)
	let [idx, name] = s:T_NextStr(a:str, a:index)
	let [idx, result] = s:T_Parse(a:str, idx)
	let result['-'] = name
	return [idx, result, s:T_TypeTransferable]
endf

fun! s:T_Parse_s(str, index)
	let [idx, str] = s:T_NextStr(a:str, a:index)
	return [idx, str, s:T_TypeString]
endf

fun! s:T_ParseNext(str, index)
	let idx = a:index
	let type = a:str[idx]
	let idx += 1
	if type == s:T_TypeNull
		return [idx+1, 0, type]
	endif
	let Handler = function('s:T_Parse_'.type)
	if empty(Handler)
		throw 'Unsupported type: '.type
	endif
	return call(Handler, [a:str, idx])
endf

fu! s:T_Parse(str, start)
	let index = a:start
	let result = {}
	while index<len(a:str)
		let [index, field] = s:T_NextStr(a:str, index)
		"echo 'Next field: '.field
		if field == ''
			"Last field
			return [index, result]
		endif
		let out = s:T_ParseNext(a:str, index)
		let index = out[0]
		let type = out[2]
		if type != s:T_TypeNull
			"echo 'Set '.field.' = '.s:T_ToString(out[1]).' /'.type
			let result[field] = out[1]
		endif
	endwhile
	throw 'Unexpected EOL'
endf

let g:Android_Parse = function('s:T_Parse')
"let [idx, result] = Android_Parse(s:TestTransferable, 0)
"echom 'Result: '.idx.' '.s:T_ToString(result)

fun! s:T_CreateObject(name)
	return {'-': a:name}
endf

fun! s:T_PutRaw(num)
	return ''.a:num.','
endf

fun! s:T_PutStr(str)
	return s:T_PutRaw(len(a:str)).substitute(a:str, "'", "'", "g").','
endf

fun! s:T_PutValue(value)
	if type(0) == type(a:value)
		return s:T_TypeNumber.s:T_PutRaw(a:value)
	endif
	if type("") == type(a:value)
		return s:T_TypeString.s:T_PutStr(a:value)
	endif
	if type(0.0) == type(a:value)
		return s:T_TypeDouble.printf('%f,', a:value)
	endif
	if type([]) == type(a:value)
		"Array
		let str = s:T_TypeList.s:T_PutRaw(len(a:value))
		for item in a:value
			let str .= s:T_PutValue(item)
		endfor
		return str
	endif
	if type({}) == type(a:value)
		if get(a:value, '-', '') != ''
			return s:T_TypeTransferable.s:T_PutStr(a:value['-']).s:T_Serialize(a:value)
		endif
		let str = s:T_TypeMap.s:T_PutRaw(len(a:value))
		for [key, value] in items(a:value)
			let str .= s:T_PutValue(key).s:T_PutValue(value)
			unlet value
		endfor
		return str
	endif
	throw 'Unsupported value: '.string(a:value)
endf

fun! s:T_Serialize(obj)
	if type({}) != type(a:obj)
		throw 'Invalid type: '.type(a:obj)
	endif
	let str = ''
	for [key, value] in items(a:obj)
		if key == '-'
			continue
		endif
		let str .= s:T_PutStr(key)
		let str .= s:T_PutValue(value)
		unlet value
	endfor
	let str .= s:T_PutStr('')
	return str
endf

fun! s:A_Execute(type, obj)
    let resp = android(a:type, s:T_Serialize(a:obj))
    let [idx, out] = s:T_Parse(resp, 0)
	if get(out, 'error', '') != ''
		throw out.error
	endif
    return out
endf

fun! g:Android_Execute(type, obj)
    return s:A_Execute(a:type, a:obj)
endf

let s:subscriptions = {}

fun! g:Android_Subscribe(obj, handler)
	if get(a:obj, 'subscription', 0) == 0
		return 0
	endif
	let s:subscriptions[a:obj.subscription] = a:handler
	return a:obj.subscription
endf

fun! s:A_OnEvent()
    "echom 'User event: '.v:android_type.' object: '.v:android_object
	if has_key(s:subscriptions, v:android_type)
		let [idx, obj] = s:T_Parse(v:android_object, 0)
		call call(s:subscriptions[v:android_type], [obj, v:android_type])
		return 1
	endif
	echom 'Invalid subscription: '.v:android_type
	return 0
endf

au User * call s:A_OnEvent()
let g:android = 1
