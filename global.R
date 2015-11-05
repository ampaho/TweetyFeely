op_pos <- scan('opinions_en/positive-words.txt', what='character', comment.char=';')
op_neg <- scan('opinions_en/negative-words.txt', what='character', comment.char=';')

pos.words <- c(op_pos, 'upgrade', 'fleek')
neg.words <- c(op_neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical', 'bs', 'bullshit', 'b.s.')

