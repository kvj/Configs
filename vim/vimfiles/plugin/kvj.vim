function! s:LoadSession(name)
	exec 'source ~/.'.a:name.'.vim'
endfunction

function! s:OpenFiles(config)
	for item in a:config
		let fname = item.name
		if 0 == stridx(fname, 'ttt:') && exists('g:tttRoot')
			"ttt files
			let fname = g:tttRoot.'/'.strpart(fname, 4)
		endif
		let fname = fnamemodify(fname, ':p')
		if !filereadable(fname)
			echom 'File not found: '.fname
			continue
		endif
		if get(item, 'pre', '') != ''
			"Have pre command
			exe item['pre']
		endif
		"echom 'Load file: '.fname.' '.get(item, 'pre').' '.get(item, 'post')
		exe 'e '.fname
		if get(item, 'post', '') != ''
			"Have post command
			exe item.post
		endif
	endfor
endfunction

"let g:kvjSession = 's'
"let g:kvjStartup = [{'name': 'ttt:00_Organizer/01.ttt'}, {'name': 'ttt:00_Organizer/07.ttt', 'pre': 'sp'}, {'name': 'ttt:01_Timeline/00.ttt', 'pre': 'tabnew'}]

if exists('g:kvjSession')
	au VimEnter * call s:LoadSession(g:kvjSession)
endif
if exists('g:kvjStartup')
	au VimEnter * call s:OpenFiles(g:kvjStartup)
endif

