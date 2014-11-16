twitterFile <- "./en/en_US.twitter.sam.txt"
twitterSampleFile <- "./final/en_US/en_US.twitter.txt"
blogsFile <- "./en/en_US.blogs.sam.txt"
blogsSampleFile <- "./final/en_US/en_US.blogs.txt"
newsFile <- "./en/en_US.news.sam.txt"
newsSampleFile <- "./final/en_US/en_US.news.txt"

sampleFile <- function(infile,outfile,header=T) {
        ci <- file(infile,"r")
        co <- file(outfile,"w")
        if (header) {
                hdr <- readLines(ci,n=1)
                writeLines(hdr,co)
        }
        recnum = 0
        numout = 0
        while (TRUE) {
                inrec <- readLines(ci,n=1)
                if (length(inrec) == 0) { # end of file?
                        close(co) 
                        return(numout)
                }
                recnum <- recnum + 1
                if (rbinom(1,1,p=.1) == 1) {
                        numout <- numout + 1
                        writeLines(inrec,co)
                }
        }
}

processFile <- function(file, enc="UTF-8" ){
        file <- readLines(file,encoding=enc)
        fileProcessed <- tm::removeWords( file, stopwords( 'english' ) )
#        fileGrams <- NGramTokenizer(fileProcessed, Weka_control(min = 2, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))
        return(fileGrams)
}

sampleFile(twitterSampleFile, twitterFile)
sampleFile(blogsSampleFile, blogsFile)
sampleFile(newsSampleFile, newsFile)

#twitterSampleEnUs <- readLines(twitterSampleFile,encoding="UTF-8")
#twitterSampleEnUsProcessed <- tm::removeWords( twitterSampleEnUs, stopwords( 'english' ) )
#twitterEnUsGrams <- NGramTokenizer(twitterSampleEnUsProcessed, Weka_control(min = 2, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))

twitterEnUs <- processFile(twitterSampleFile)
blogsEnUs <- processFile(blogsSampleFile)
newsEnUs <- processFile(newsSampleFile)


###Exploratory Analysis
