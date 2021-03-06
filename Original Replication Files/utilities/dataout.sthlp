{smcl}
{* 16oct2009{...}
{cmd:help dataout}

{hline}

{title:Title}

{p2colset 5 16 22 2}{...}
{p2col :{hi:  dataout} {hline 2}}Exports dataset or tab-delimited file into various formats{p_end}

{marker s_Syntax}
{title:Syntax}

{p 4 4 6}
{cmdab:dataout} <using filename> [, {it:options}]

{marker s_Description}
{title:Description}

{p 4 4 6}
{cmd:dataout} provides a fast and easy way to export dataset or tab-delimited files into various output 
formats compatible with Latex, Word, and Excel.

{p 4 4 6}Specify {opt <using filename>} if converting external tab-delimited file.

{p 4 4 6}Specify {opt save( )} if converting the current dataset in memory.


{marker s_Options}
{title:Options}

{dlgtab:Main}

{p 4 12 6}{opt save( )} Must be specified when <using file> is not specified. {p_end}

{p 4 12 6}{opt replace( )} Replace pre-exiting files. {p_end}

{p 4 12 6}{opt tex} Convert into Latex or tex compatible file. {p_end}

{p 4 12 6}{opt excel} Convert into Excel compatible file. {p_end}

{p 4 12 6}{opt word} Convert into Word compatible file. {p_end}

{p 4 12 6}{opt nohead} Force variable names not to be reported. {p_end}

{p 4 12 6}{opt head} Force column head to be reported. {p_end}

{p 4 12 6}{opt auto(#)} where # is the number of automatically formatted digits to be reported. {p_end}

{p 4 12 6}{opt noauto} No automatic formating. {p_end}

{p 4 12 6}{opt dec(#)} where # is the fixed number digits to be reported. {p_end}

{p 4 12 6}{opt mid:border(#)} where # is the location of middle border. Default is mid(1). {p_end}


{marker s_0}
{title:Examples}


{p 4 4 6}* exporting dataset in memory{p_end}
{p 4 4 6}{stata sysuse auto, clear}{p_end}
{p 4 4 6}{stata dataout, save(myfile) word tex excel replace}{p_end}


{p 4 4 6}* collapse{p_end}
{p 4 4 6}{stata collapse (mean) mpg headroom price weight, by(turn)}{p_end}
{p 4 4 6}{stata replace mpg=round(mpg,1)}{p_end}
{p 4 4 6}{stata dataout, save(myfile) word tex excel replace}{p_end}


{p 4 4 6}* contract{p_end}
{p 4 4 6}{stata sysuse auto, clear}{p_end}
{p 4 4 6}{stata contract foreign rep78, p(percentage) cf(cumfreq) cp(cumperc)}{p_end}
{p 4 4 6}{stata dataout, save(myfile) tex replace excel word dec(2)}{p_end}


{p 4 4 6}* exporting any tab-delimited text file{p_end}
{p 4 4 6}{stata dataout using myfile.txt, word tex excel replace mid(2)}{p_end}


{p 4 4 6}* exporting your outreg/outreg2 file{p_end}
{p 4 4 6}{stata sysuse auto, clear}{p_end}
{p 4 4 6}{stata reg price rep head trunk}{p_end}
{p 4 4 6}{stata outreg using myfile.out, replace}{p_end}
{p 4 4 6}{stata reg price rep head turn gear}{p_end}
{p 4 4 6}{stata outreg using myfile.out, append}{p_end}
{p 4 4 6}{stata dataout using myfile.out, word tex excel replace mid(2)}{p_end}


{title:Acknowledgements}

{p 4 12 6}The codes for word and tex are traced back to John Gallup's outreg through outreg2.{p_end}
{p 4 12 6}excel is implemented as a facsimile of xmlsave.{p_end}


{title:Remarks}

{p 4 12 6}The decimal format is currently set as fc.{p_end}


{title:Author}

{p 4 4 6}Roy Wada{p_end}
{p 4 4 6}roywada@hotmail.com{p_end}

