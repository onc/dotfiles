$pdf_mode = 1;
$pdflatex = 'pdflatex -8bit -etex -file-line-error -halt-on-error -synctex=1 %O %S';
$pdf_previewer = 'okular %S';
$pdf_update_method = 0;
$clean_ext = "syntex.gz";
add_cus_dep('glo', 'gls', 0, 'makeglo2gls');
sub makeglo2gls {
    system("makeindex -s '$_[0]'.ist -t '$_[0]'.glg -o '$_[0]'.gls '$_[0]'.glo");
}
