let signTask = 22000
let dmFormat = '%m/%d'
let dmyFileFormat = '%y%m%d'
let hmFormat = '%H:%M'
let rxTime = '\(\d\{1,2\}\):\(\d\{2\}\)' "H:mm
let rxDate = '\(\(\d\{2\}\)/\)\?\(\d\{1,2\}\)/\(\d\{1,2\}\)' "yy/m/d

fun! Log(...)
	let buf = ''
	let idx = 0
	for item in a:000
		if idx>0
			let buf .= ', '
		endif
		let buf .= item
		let idx += 1
	endfor
	echom buf
endf

fun! ttt#showReport()
	call Log('showReport', 10)
endf
