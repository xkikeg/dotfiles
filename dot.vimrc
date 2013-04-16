"タブ幅の設定
set tabstop=4
set shiftwidth=4
"全角記号でずれないようにする。
set ambiwidth=double
"新しい行のインデントを
set cindent
"ファイル保存ダイアログの初期ディレクトリをバッファファイル位置に設定
set browsedir=buffer
"色付けの設定をON
syntax on
"行番号の表示
set number
set incsearch

"空白文字の表示
set list
set listchars=tab:>-,trail:_,eol:↲

"Use UTF-8
set termencoding=utf-8
set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,shift-jis,euc-jp
"set fileencoding=utf-8
"set fileencodings=iso-2022-jp,utf-8,euc-jp,ucs-2le,ucs-2,cp932

"Cursor lineを放置・ウィンドウ移動時だけ表示
"From: http://d.hatena.ne.jp/thinca/20090530/1243615055
augroup vimrc-auto-cursorline
	autocmd!
	autocmd CursorMoved,CursorMovedI * call s:auto_cursorline('CursorMoved')
	autocmd CursorHold,CursorHoldI * call s:auto_cursorline('CursorHold')
	autocmd WinEnter * call s:auto_cursorline('WinEnter')
	autocmd WinLeave * call s:auto_cursorline('WinLeave')

	let s:cursorline_lock = 0
	function! s:auto_cursorline(event)
		if a:event ==# 'WinEnter'
			setlocal cursorline
			let s:cursorline_lock = 2
		elseif a:event ==# 'WinLeave'
			setlocal nocursorline
		elseif a:event ==# 'CursorMoved'
			if s:cursorline_lock
				if 1 < s:cursorline_lock
					let s:cursorline_lock = 1
				else
					setlocal nocursorline
					let s:cursorline_lock = 0
				endif
			endif
		elseif a:event ==# 'CursorHold'
		setlocal cursorline
		let s:cursorline_lock = 1
		endif
	endfunction
augroup END
