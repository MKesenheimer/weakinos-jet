c############### Flags.h ###############################################
c last modified by MK, 11.07.2018

c store here the flags which are necessary for this process
        
        ! whether to use fake virtuals or not
        logical flg_fakevirtuals

        ! flg_btilde: if flg_split!=0: 
        ! distinguish real contributions for btilde and remnants...
        !logical flg_btilde

        common/flags/ flg_fakevirtuals
        save /flags/

c############### end Flags.h ###########################################
