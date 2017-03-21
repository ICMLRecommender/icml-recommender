# PDF to DAT converter

Takes as input a file mask of PDF files, extracts the text from them. From this text it builds a matrix of per-file counts limiting the feature counts and then saves this data in the format expected by the CTR code from (https://github.com/blei-lab/ctr).

Fully python solution, no external dependencies except standard python libs.

# Installation
sudo pip install -r requirements.txt

# Usage

To convert pdf to txt:
>  python pdfconversion.py -p icml2016/*.pdf -t icml2016/txt_files -m pdf2txt

To convert txt to dat:
>  python pdfconversion.py -t icml2016/txt_files -m txt2dat

Full example:
> python pdfconversion.py -p '../icml2016/*.pdf' -t ../icml2016/txt_files/ -m full
