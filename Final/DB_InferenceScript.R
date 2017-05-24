rm(list=ls())
gc()

require(TInPosition)

fast.csci <- function(DATA, REGRESSORS=NULL, rows, center=T, scale=F, main.effects.only=F, impute=T){
  
  ## center and/or scale the data before hand.
  DATA <- expo.scale(DATA[rows,],center=center,scale=scale)
  
  ## correct (residualize) the data by confounds
  if(!is.null(REGRESSORS)){
    if(main.effects.only){
      this.formula <- "x~."
    }else{
      this.formula <- paste0("x~.^",ncol(REGRESSORS[rows,]))
    }	  
    DATA <- apply(
      DATA,
      2,
      function(x){ 
        resid( lm( as.formula(this.formula), data = REGRESSORS[rows,], na.action = na.exclude) )
      }
    )
  }
  
  
  
  ## impute to mean
  if(impute){
    mean.impute(DATA)
  }
  
  ## replace tiny values by 0
  DATA[which( abs(DATA) < .Machine$double.eps )] <- 0	
  return(DATA)
}

mean.impute <- function(DATA){
  DATA <- apply(DATA,2,function(x){ x[(is.na(x))] <- mean(x,na.rm = T); x })
  return(DATA)
}

escofier.transform <- function(DAT,center=T,scale=T){
  
  scaled.DAT <- expo.scale(DAT,center=center,scale=scale)
  escofier.DAT <- cbind((1-scaled.DAT)/2,(1+ scaled.DAT)/2)
  colnames(escofier.DAT) <- c(paste0(colnames(scaled.DAT),"-"),paste0(colnames(scaled.DAT),"+"))
  return(escofier.DAT)
  
}


load('./fin.des.rda')
load('./fin.des.nom.rda')
load('./fin.outs.rda')
load('./fin.preds.rda')



fin.preds$studysex <- ifelse(fin.preds$studysex==1,"MALE","FEMALE")
fin.preds$studysex <- as.factor(fin.preds$studysex)

fin.preds$clinage <- ifelse(fin.preds$clinage>=777,NA,fin.preds$clinage)
fin.preds$clinage <- as.numeric(fin.preds$clinage)

fin.preds$studyed <- ifelse(fin.preds$studyed>=99,NA,fin.preds$studyed)
fin.preds$studyed <- as.numeric(fin.preds$studyed) ## consider factorizing this if distribution is weird.

fin.preds$cgsex <- ifelse(fin.preds$cgsex==1,"MALE","FEMALE")
fin.preds$cgsex <- as.factor(fin.preds$cgsex)

fin.preds$cgage <- ifelse(fin.preds$cgage==999,NA,fin.preds$cgage)
fin.preds$cgage <- as.numeric(fin.preds$cgage)

fin.preds$cgedyrs <- ifelse(fin.preds$cgedyrs>=88,NA,fin.preds$cgedyrs)
fin.preds$cgedyrs <- as.numeric(fin.preds$cgedyrs)

fin.preds$cgedlev <- ifelse(fin.preds$cgedlev>=77,NA,fin.preds$cgedlev)
fin.preds$cgedlev[fin.preds$cgedlev==1 | fin.preds$cgedlev==2 | fin.preds$cgedlev==3 | fin.preds$cgedlev==4] <- "<HS"
fin.preds$cgedlev[fin.preds$cgedlev==5] <- "HS"
fin.preds$cgedlev[fin.preds$cgedlev==6 | fin.preds$cgedlev==7 | fin.preds$cgedlev==8] <- "<BACHELORS"
fin.preds$cgedlev[fin.preds$cgedlev==9] <- "BACHELORS"
fin.preds$cgedlev[fin.preds$cgedlev==10] <- "MASTERS"  
fin.preds$cgedlev[fin.preds$cgedlev==11] <- "PHD"    
fin.preds$cgedlev[fin.preds$cgedlev==12] <- "OTHER"  
fin.preds$cgedlev <- as.factor(fin.preds$cgedlev)


fin.preds$apathy <- ifelse(fin.preds$apathy >= 5, NA, fin.preds$apathy)
fin.preds$apathy[fin.preds$apathy==1] <- "YES"
fin.preds$apathy[fin.preds$apathy==2] <- "MAYBE"
fin.preds$apathy[fin.preds$apathy==3] <- "NO"
fin.preds$apathy[fin.preds$apathy==4] <- "NOT_RELEVANT"
fin.preds$apathy <- as.factor(fin.preds$apathy)

fin.preds$wander <- ifelse(fin.preds$wander >= 5, NA, fin.preds$wander)
fin.preds$wander[fin.preds$wander==1] <- "YES"
fin.preds$wander[fin.preds$wander==2] <- "MAYBE"
fin.preds$wander[fin.preds$wander==3] <- "NO"
fin.preds$wander[fin.preds$wander==4] <- "NOT_RELEVANT"
fin.preds$wander <- as.factor(fin.preds$wander)

fin.preds$violenc <- ifelse(fin.preds$violenc >= 5, NA, fin.preds$violenc)
fin.preds$violenc[fin.preds$violenc==1] <- "YES"
fin.preds$violenc[fin.preds$violenc==2] <- "MAYBE"
fin.preds$violenc[fin.preds$violenc==3] <- "NO"
fin.preds$violenc[fin.preds$violenc==4] <- "NOT_RELEVANT"
fin.preds$violenc <- as.factor(fin.preds$violenc)

fin.preds$disinhi <- ifelse(fin.preds$disinhi >= 5, NA, fin.preds$disinhi)
fin.preds$disinhi[fin.preds$disinhi==1] <- "YES"
fin.preds$disinhi[fin.preds$disinhi==2] <- "MAYBE"
fin.preds$disinhi[fin.preds$disinhi==3] <- "NO"
fin.preds$disinhi[fin.preds$disinhi==4] <- "NOT_RELEVANT"
fin.preds$disinhi <- as.factor(fin.preds$disinhi)

fin.preds$otherbe <- ifelse(fin.preds$otherbe >= 5, NA, fin.preds$otherbe)
fin.preds$otherbe[fin.preds$otherbe==1] <- "YES"
fin.preds$otherbe[fin.preds$otherbe==2] <- "MAYBE"
fin.preds$otherbe[fin.preds$otherbe==3] <- "NO"
fin.preds$otherbe[fin.preds$otherbe==4] <- "NOT_RELEVANT"
fin.preds$otherbe <- as.factor(fin.preds$otherbe)

fin.preds$sad <- ifelse(fin.preds$sad >= 8, NA, fin.preds$sad)
fin.preds$sad[fin.preds$sad==0] <- "NO"
fin.preds$sad[fin.preds$sad==1] <- "YES"
fin.preds$sad <- as.factor(fin.preds$sad)

fin.preds$interest <- ifelse(fin.preds$interest >= 8, NA, fin.preds$interest)
fin.preds$interest[fin.preds$interest==0] <- "NO"
fin.preds$interest[fin.preds$interest==1] <- "YES"
fin.preds$interest <- as.factor(fin.preds$interest)

fin.preds$appetite <- ifelse(fin.preds$appetite >= 8, NA, fin.preds$appetite)
fin.preds$appetite[fin.preds$appetite==0] <- "NO"
fin.preds$appetite[fin.preds$appetite==1] <- "YES"
fin.preds$appetite <- as.factor(fin.preds$appetite)

fin.preds$lossweig <- ifelse(fin.preds$lossweig >= 8, NA, fin.preds$lossweig)
fin.preds$lossweig[fin.preds$lossweig==0] <- "NO"
fin.preds$lossweig[fin.preds$lossweig==1] <- "YES"
fin.preds$lossweig <- as.factor(fin.preds$lossweig)

fin.preds$asleep <- ifelse(fin.preds$asleep >= 8, NA, fin.preds$asleep)
fin.preds$asleep[fin.preds$asleep==0] <- "NO"
fin.preds$asleep[fin.preds$asleep==1] <- "YES"
fin.preds$asleep <- as.factor(fin.preds$asleep)

fin.preds$awaken <- ifelse(fin.preds$awaken >= 8, NA, fin.preds$awaken)
fin.preds$awaken[fin.preds$awaken==0] <- "NO"
fin.preds$awaken[fin.preds$awaken==1] <- "YES"
fin.preds$awaken <- as.factor(fin.preds$awaken)

fin.preds$allday <- ifelse(fin.preds$allday >= 8, NA, fin.preds$allday)
fin.preds$allday[fin.preds$allday==0] <- "NO"
fin.preds$allday[fin.preds$allday==1] <- "YES"
fin.preds$allday <- as.factor(fin.preds$allday)

fin.preds$tiredall <- ifelse(fin.preds$tiredall >= 8, NA, fin.preds$tiredall)
fin.preds$tiredall[fin.preds$tiredall==0] <- "NO"
fin.preds$tiredall[fin.preds$tiredall==1] <- "YES"
fin.preds$tiredall <- as.factor(fin.preds$tiredall)

fin.preds$move <- ifelse(fin.preds$move >= 8, NA, fin.preds$move)
fin.preds$move[fin.preds$move==0] <- "NO"
fin.preds$move[fin.preds$move==1] <- "YES"
fin.preds$move <- as.factor(fin.preds$move)

fin.preds$worthles <- ifelse(fin.preds$worthles >= 8, NA, fin.preds$worthles)
fin.preds$worthles[fin.preds$worthles==0] <- "NO"
fin.preds$worthles[fin.preds$worthles==1] <- "YES"
fin.preds$worthles <- as.factor(fin.preds$worthles)

fin.preds$suicide <- ifelse(fin.preds$suicide >= 8, NA, fin.preds$suicide)
fin.preds$suicide[fin.preds$suicide==0] <- "NO"
fin.preds$suicide[fin.preds$suicide==1] <- "YES"
fin.preds$suicide <- as.factor(fin.preds$suicide)

fin.preds$impress <- ifelse(fin.preds$impress >= 5, NA, fin.preds$impress)
fin.preds$impress[fin.preds$impress==1] <- "YES"
fin.preds$impress[fin.preds$impress==2] <- "MAYBE"
fin.preds$impress[fin.preds$impress==3] <- "NO"
fin.preds$impress[fin.preds$impress==4] <- "NOT_RELEVANT"
fin.preds$impress <- as.factor(fin.preds$impress)

fin.preds$shorterm <- ifelse(fin.preds$shorterm >= 8, NA, fin.preds$shorterm)
fin.preds$shorterm[fin.preds$shorterm==0] <- "NO"
fin.preds$shorterm[fin.preds$shorterm==1] <- "YES"
fin.preds$shorterm <- as.factor(fin.preds$shorterm)

fin.preds$longterm <- ifelse(fin.preds$longterm >= 8, NA, fin.preds$longterm)
fin.preds$longterm[fin.preds$longterm==0] <- "NO"
fin.preds$longterm[fin.preds$longterm==1] <- "YES"
fin.preds$longterm <- as.factor(fin.preds$longterm)

fin.preds$abstract <- ifelse(fin.preds$abstract >= 8, NA, fin.preds$abstract)
fin.preds$abstract[fin.preds$abstract==0] <- "NO"
fin.preds$abstract[fin.preds$abstract==1] <- "YES"
fin.preds$abstract <- as.factor(fin.preds$abstract)

fin.preds$judgemen <- ifelse(fin.preds$judgemen >= 8, NA, fin.preds$judgemen)
fin.preds$judgemen[fin.preds$judgemen==0] <- "NO"
fin.preds$judgemen[fin.preds$judgemen==1] <- "YES"
fin.preds$judgemen <- as.factor(fin.preds$judgemen)

fin.preds$aphasia <- ifelse(fin.preds$aphasia >= 8, NA, fin.preds$aphasia)
fin.preds$aphasia[fin.preds$aphasia==0] <- "NO"
fin.preds$aphasia[fin.preds$aphasia==1] <- "YES"
fin.preds$aphasia <- as.factor(fin.preds$aphasia)

fin.preds$apraxia <- ifelse(fin.preds$apraxia >= 8, NA, fin.preds$apraxia)
fin.preds$apraxia[fin.preds$apraxia==0] <- "NO"
fin.preds$apraxia[fin.preds$apraxia==1] <- "YES"
fin.preds$apraxia <- as.factor(fin.preds$apraxia)

fin.preds$change <- ifelse(fin.preds$change >= 8, NA, fin.preds$change)
fin.preds$change[fin.preds$change==0] <- "NO"
fin.preds$change[fin.preds$change==1] <- "YES"
fin.preds$change <- as.factor(fin.preds$change)

fin.preds$work <- ifelse(fin.preds$work >= 8, NA, fin.preds$work)
fin.preds$work[fin.preds$work==0] <- "NO"
fin.preds$work[fin.preds$work==1] <- "YES"
fin.preds$work <- as.factor(fin.preds$work)

fin.preds$social <- ifelse(fin.preds$social >= 8, NA, fin.preds$social)
fin.preds$social[fin.preds$social==0] <- "NO"
fin.preds$social[fin.preds$social==1] <- "YES"
fin.preds$social <- as.factor(fin.preds$social)

fin.preds$relation <- ifelse(fin.preds$relation >= 8, NA, fin.preds$relation)
fin.preds$relation[fin.preds$relation==0] <- "NO"
fin.preds$relation[fin.preds$relation==1] <- "YES"
fin.preds$relation <- as.factor(fin.preds$relation)

fin.preds$dbdscore <- ifelse(fin.preds$dbdscore == 99, NA, fin.preds$dbdscore)
fin.preds$dbdscore <- as.numeric(fin.preds$dbdscore)

fin.preds$adlrate[fin.preds$adlrate==2] <- "EXCELLENT.or.GOOD"
fin.preds$adlrate[fin.preds$adlrate==3] <- "MILD.IMPAIRMENT"
fin.preds$adlrate[fin.preds$adlrate==4] <- "MODERATE.IMPAIRMENT"
fin.preds$adlrate[fin.preds$adlrate==5] <- "SEVERE.IMPAIRMENT"
fin.preds$adlrate[fin.preds$adlrate==6] <- "TOTAL.IMPAIRMENT"
fin.preds$adlrate[fin.preds$adlrate==9] <- NA
fin.preds$adlrate <- as.factor(fin.preds$adlrate)

fin.preds$c238 <- ifelse(fin.preds$c238 >= 8, NA, fin.preds$c238)
fin.preds$c238[fin.preds$c238==0] <- "NO"
fin.preds$c238[fin.preds$c238==1] <- "YES"
fin.preds$c238 <- as.factor(fin.preds$c238)

fin.preds$c239 <- ifelse(fin.preds$c239 >= 8, NA, fin.preds$c239)
fin.preds$c239[fin.preds$c239==0] <- "NO"
fin.preds$c239[fin.preds$c239==1] <- "YES"
fin.preds$c239 <- as.factor(fin.preds$c239)

fin.preds$c240 <- ifelse(fin.preds$c240 >= 8, NA, fin.preds$c240)
fin.preds$c240[fin.preds$c240==0] <- "NO"
fin.preds$c240[fin.preds$c240==1] <- "YES"
fin.preds$c240 <- as.factor(fin.preds$c240)

fin.preds$c241 <- ifelse(fin.preds$c241 >= 8, NA, fin.preds$c241)
fin.preds$c241[fin.preds$c241==0] <- "NO"
fin.preds$c241[fin.preds$c241==1] <- "LESS"
fin.preds$c241[fin.preds$c241==2] <- "MORE"
fin.preds$c241 <- as.factor(fin.preds$c241)

fin.preds$c242 <- ifelse(fin.preds$c242 >= 8, NA, fin.preds$c242)
fin.preds$c242[fin.preds$c242==0] <- "NO"
fin.preds$c242[fin.preds$c242==1] <- "LESS"
fin.preds$c242[fin.preds$c242==2] <- "MORE"
fin.preds$c242 <- as.factor(fin.preds$c242)

fin.preds$c243 <- ifelse(fin.preds$c243 >= 8, NA, fin.preds$c243)
fin.preds$c243[fin.preds$c243==0] <- "NO"
fin.preds$c243[fin.preds$c243==1] <- "YES"
fin.preds$c243 <- as.factor(fin.preds$c243)

fin.preds$c244 <- ifelse(fin.preds$c244 >= 8, NA, fin.preds$c244)
fin.preds$c244[fin.preds$c244==0] <- "NO"
fin.preds$c244[fin.preds$c244==1] <- "LESS"
fin.preds$c244[fin.preds$c244==2] <- "MORE"
fin.preds$c244 <- as.factor(fin.preds$c244)

fin.preds$c246 <- ifelse(fin.preds$c246 >= 8, NA, fin.preds$c246)
fin.preds$c246[fin.preds$c246==0] <- "GRADUAL"
fin.preds$c246[fin.preds$c246==1] <- "SUDDEN"
fin.preds$c246[fin.preds$c246==7] <- "NONE"
fin.preds$c246 <- as.factor(fin.preds$c246)

fin.preds$c247 <- ifelse(fin.preds$c247 >= 8, NA, fin.preds$c247)
fin.preds$c247[fin.preds$c247==0] <- "NO"
fin.preds$c247[fin.preds$c247==1] <- "SLIGHT"
fin.preds$c247[fin.preds$c247==2] <- "GREAT"
fin.preds$c247 <- as.factor(fin.preds$c247)

fin.preds$c248 <- ifelse(fin.preds$c248 >= 8, NA, fin.preds$c248)
fin.preds$c248[fin.preds$c248==0] <- "NO"
fin.preds$c248[fin.preds$c248==1] <- "SLIGHT"
fin.preds$c248[fin.preds$c248==2] <- "GREAT"
fin.preds$c248 <- as.factor(fin.preds$c248)

fin.preds$c249 <- ifelse(fin.preds$c249 >= 8, NA, fin.preds$c249)
fin.preds$c249[fin.preds$c249==0] <- "NO"
fin.preds$c249[fin.preds$c249==1] <- "SLIGHT"
fin.preds$c249[fin.preds$c249==2] <- "GREAT"
fin.preds$c249 <- as.factor(fin.preds$c249)

fin.preds$c250 <- ifelse(fin.preds$c250 >= 8, NA, fin.preds$c250)
fin.preds$c250[fin.preds$c250==0] <- "NO"
fin.preds$c250[fin.preds$c250==1] <- "SLIGHT"
fin.preds$c250[fin.preds$c250==2] <- "GREAT"
fin.preds$c250 <- as.factor(fin.preds$c250)

fin.preds$c254 <- ifelse(fin.preds$c254 >= 8, NA, fin.preds$c254)
fin.preds$c254[fin.preds$c254==0] <- "NO"
fin.preds$c254[fin.preds$c254==1] <- "YES"
fin.preds$c254 <- as.factor(fin.preds$c254)

fin.preds$c255 <- ifelse(fin.preds$c255 >= 8, NA, fin.preds$c255)
fin.preds$c255[fin.preds$c255==0] <- "NO"
fin.preds$c255[fin.preds$c255==1] <- "SOMETIMES"
fin.preds$c255[fin.preds$c255==2] <- "OFTEN"
fin.preds$c255 <- as.factor(fin.preds$c255)

fin.preds$c256 <- ifelse(fin.preds$c256 >= 8, NA, fin.preds$c256)
fin.preds$c256[fin.preds$c256==0] <- "NO"
fin.preds$c256[fin.preds$c256==1] <- "YES"
fin.preds$c256 <- as.factor(fin.preds$c256)

fin.preds$c257 <- ifelse(fin.preds$c257 >= 8, NA, fin.preds$c257)
fin.preds$c257[fin.preds$c257==0] <- "NO"
fin.preds$c257[fin.preds$c257==1] <- "YES"
fin.preds$c257 <- as.factor(fin.preds$c257)

fin.preds$c258 <- ifelse(fin.preds$c258 >= 8, NA, fin.preds$c258)
fin.preds$c258[fin.preds$c258==0] <- "NO"
fin.preds$c258[fin.preds$c258==1] <- "YES"
fin.preds$c258 <- as.factor(fin.preds$c258)

fin.preds$c259 <- ifelse(fin.preds$c259 >= 8, NA, fin.preds$c259)
fin.preds$c259[fin.preds$c259==0] <- "NO"
fin.preds$c259[fin.preds$c259==1] <- "YES"
fin.preds$c259 <- as.factor(fin.preds$c259)

fin.preds$c261 <- ifelse(fin.preds$c261 >= 8, NA, fin.preds$c261)
fin.preds$c261[fin.preds$c261==0] <- "GRADUAL"
fin.preds$c261[fin.preds$c261==1] <- "SUDDEN"
fin.preds$c261[fin.preds$c261==7] <- "NONE"
fin.preds$c261 <- as.factor(fin.preds$c261)

fin.preds$c262 <- ifelse(fin.preds$c262 >= 8, NA, fin.preds$c262)
fin.preds$c262[fin.preds$c262==0] <- "GRADUAL"
fin.preds$c262[fin.preds$c262==1] <- "SUDDEN"
fin.preds$c262[fin.preds$c262==7] <- "NONE"
fin.preds$c262 <- as.factor(fin.preds$c262)

fin.preds$c263 <- ifelse(fin.preds$c263 >= 8, NA, fin.preds$c263)
fin.preds$c263[fin.preds$c263==0] <- "NO"
fin.preds$c263[fin.preds$c263==1] <- "SLIGHT"
fin.preds$c263[fin.preds$c263==2] <- "GREAT"
fin.preds$c263 <- as.factor(fin.preds$c263)

fin.preds$c264 <- ifelse(fin.preds$c264 >= 8, NA, fin.preds$c264)
fin.preds$c264[fin.preds$c264==0] <- "NO"
fin.preds$c264[fin.preds$c264==1] <- "SLIGHT"
fin.preds$c264[fin.preds$c264==2] <- "GREAT"
fin.preds$c264 <- as.factor(fin.preds$c264)

fin.preds$c265[fin.preds$c265>=8] <- NA
fin.preds$c265[fin.preds$c265==0] <- "NO"
fin.preds$c265[fin.preds$c265==2] <- "MESSY"
fin.preds$c265[fin.preds$c265==4] <- "SIMPLE.SOLIDS"
fin.preds$c265[fin.preds$c265==6] <- "HAS.TO.BE.FED"
fin.preds$c265 <- as.factor(fin.preds$c265)

fin.preds$c266[fin.preds$c266>=8] <- NA
fin.preds$c266[fin.preds$c266==0] <- "NO"
fin.preds$c266[fin.preds$c266==2] <- "OCCASIONAL"
fin.preds$c266[fin.preds$c266==4] <- "WRONG"
fin.preds$c266[fin.preds$c266==6] <- "UNABLE"
fin.preds$c266 <- as.factor(fin.preds$c266)

fin.preds$c267[fin.preds$c267>=8] <- NA
fin.preds$c267[fin.preds$c267==0] <- "NO"
fin.preds$c267[fin.preds$c267==2] <- "OCCASIONAL"
fin.preds$c267[fin.preds$c267==4] <- "OFTEN"
fin.preds$c267[fin.preds$c267==6] <- "DOUBLY"
fin.preds$c267 <- as.factor(fin.preds$c267)

fin.preds$c269 <- ifelse(fin.preds$c269 >= 8, NA, fin.preds$c269)
fin.preds$c269[fin.preds$c269==0] <- "GRADUAL"
fin.preds$c269[fin.preds$c269==1] <- "SUDDEN"
fin.preds$c269[fin.preds$c269==7] <- "NONE"
fin.preds$c269 <- as.factor(fin.preds$c269)

fin.preds$c275 <- ifelse(fin.preds$c275 >= 8, NA, fin.preds$c275)
fin.preds$c275[fin.preds$c275==0] <- "NO"
fin.preds$c275[fin.preds$c275==1] <- "YES"
fin.preds$c275 <- as.factor(fin.preds$c275)

fin.preds$c276 <- ifelse(fin.preds$c276 >= 8, NA, fin.preds$c276)
fin.preds$c276[fin.preds$c276==0] <- "NO"
fin.preds$c276[fin.preds$c276==1] <- "YES"
fin.preds$c276 <- as.factor(fin.preds$c276)

fin.preds$c277 <- ifelse(fin.preds$c277 >= 8, NA, fin.preds$c277)
fin.preds$c277[fin.preds$c277==0] <- "NO"
fin.preds$c277[fin.preds$c277==1] <- "YES"
fin.preds$c277 <- as.factor(fin.preds$c277)

fin.preds$c280 <- ifelse(fin.preds$c280 >= 8, NA, fin.preds$c280)
fin.preds$c280[fin.preds$c280==0] <- "NO"
fin.preds$c280[fin.preds$c280==1] <- "YES"
fin.preds$c280 <- as.factor(fin.preds$c280)

fin.preds$c281 <- ifelse(fin.preds$c281 >= 8, NA, fin.preds$c281)
fin.preds$c281[fin.preds$c281==0] <- "NO"
fin.preds$c281[fin.preds$c281==1] <- "YES"
fin.preds$c281 <- as.factor(fin.preds$c281)

fin.preds$c282 <- ifelse(fin.preds$c282 >= 8, NA, fin.preds$c282)
fin.preds$c282[fin.preds$c282==0] <- "NO"
fin.preds$c282[fin.preds$c282==1] <- "YES"
fin.preds$c282 <- as.factor(fin.preds$c282)

fin.preds$c283 <- ifelse(fin.preds$c283 >= 8, NA, fin.preds$c283)
fin.preds$c283[fin.preds$c283==0] <- "NO"
fin.preds$c283[fin.preds$c283==1] <- "YES"
fin.preds$c283 <- as.factor(fin.preds$c283)

fin.preds$c286 <- ifelse(fin.preds$c286 >= 8, NA, fin.preds$c286)
fin.preds$c286[fin.preds$c286==0] <- "NO"
fin.preds$c286[fin.preds$c286==1] <- "YES"
fin.preds$c286 <- as.factor(fin.preds$c286)

fin.preds$c287 <- ifelse(fin.preds$c287 >= 8, NA, fin.preds$c287)
fin.preds$c287[fin.preds$c287==0] <- "NO"
fin.preds$c287[fin.preds$c287==1] <- "YES"
fin.preds$c287 <- as.factor(fin.preds$c287)

fin.preds$c288 <- ifelse(fin.preds$c288 >= 8, NA, fin.preds$c288)
fin.preds$c288[fin.preds$c288==0] <- "NO"
fin.preds$c288[fin.preds$c288==1] <- "YES"
fin.preds$c288 <- as.factor(fin.preds$c288)

fin.preds$c289 <- ifelse(fin.preds$c289 >= 8, NA, fin.preds$c289)
fin.preds$c289[fin.preds$c289==0] <- "NO"
fin.preds$c289[fin.preds$c289==1] <- "YES"
fin.preds$c289 <- as.factor(fin.preds$c289)

fin.preds$c290 <- ifelse(fin.preds$c290 >= 8, NA, fin.preds$c290)
fin.preds$c290[fin.preds$c290==0] <- "NO"
fin.preds$c290[fin.preds$c290==1] <- "YES"
fin.preds$c290 <- as.factor(fin.preds$c290)

fin.preds$c292 <- ifelse(fin.preds$c292 >= 7, NA, fin.preds$c292)
fin.preds$c292[fin.preds$c292==0] <- "NO"
fin.preds$c292[fin.preds$c292==1] <- "SOME"
fin.preds$c292[fin.preds$c292==2] <- "GREAT"
fin.preds$c292 <- as.factor(fin.preds$c292)

fin.preds$c301 <- ifelse(fin.preds$c301 >= 8, NA, fin.preds$c301)
fin.preds$c301[fin.preds$c301==0] <- "NO"
fin.preds$c301[fin.preds$c301==1] <- "ONE"
fin.preds$c301[fin.preds$c301==2] <- "TWO"
fin.preds$c301[fin.preds$c301==3] <- "THREE"
fin.preds$c301 <- as.factor(fin.preds$c301)

fin.preds$c302 <- ifelse(fin.preds$c302 >= 8, NA, fin.preds$c302)
fin.preds$c302[fin.preds$c302==0] <- "NO"
fin.preds$c302[fin.preds$c302==1] <- "INFANTILE"
fin.preds$c302[fin.preds$c302==2] <- "PAST"
fin.preds$c302[fin.preds$c302==3] <- "CURRENT"
fin.preds$c302 <- as.factor(fin.preds$c302)

fin.preds$c303 <- ifelse(fin.preds$c303 >= 8, NA, fin.preds$c303)
fin.preds$c303[fin.preds$c303==0] <- "NO"
fin.preds$c303[fin.preds$c303==1] <- "YES"
fin.preds$c303 <- as.factor(fin.preds$c303)

fin.preds$c304 <- ifelse(fin.preds$c304 >= 8, NA, fin.preds$c304)
fin.preds$c304[fin.preds$c304==0] <- "NO"
fin.preds$c304[fin.preds$c304==1] <- "YES"
fin.preds$c304 <- as.factor(fin.preds$c304)

fin.preds$c305 <- ifelse(fin.preds$c305 >= 8, NA, fin.preds$c305)
fin.preds$c305[fin.preds$c305==0] <- "NO"
fin.preds$c305[fin.preds$c305==1] <- "YES"
fin.preds$c305 <- as.factor(fin.preds$c305)

fin.preds$c306 <- ifelse(fin.preds$c306 >= 8, NA, fin.preds$c306)
fin.preds$c306[fin.preds$c306==0] <- "NO"
fin.preds$c306[fin.preds$c306==1] <- "YES"
fin.preds$c306 <- as.factor(fin.preds$c306)

fin.preds$c307 <- ifelse(fin.preds$c307 >= 8, NA, fin.preds$c307)
fin.preds$c307[fin.preds$c307==0] <- "NO"
fin.preds$c307[fin.preds$c307==1] <- "TRANQ"
fin.preds$c307[fin.preds$c307==2] <- "HYP"
fin.preds$c307[fin.preds$c307==3] <- "BARB"
fin.preds$c307[fin.preds$c307==4] <- "STIM"
fin.preds$c307[fin.preds$c307==5] <- "OTHER"
fin.preds$c307 <- as.factor(fin.preds$c307)

fin.preds$c308 <- ifelse(fin.preds$c308 >= 8, NA, fin.preds$c308)
fin.preds$c308[fin.preds$c308==0] <- "NO"
fin.preds$c308[fin.preds$c308==1] <- "YES"
fin.preds$c308 <- as.factor(fin.preds$c308)

fin.preds$c309 <- ifelse(fin.preds$c309 >= 8, NA, fin.preds$c309)
fin.preds$c309[fin.preds$c309==0] <- "NO"
fin.preds$c309[fin.preds$c309 > 0 & fin.preds$c309 <= 6] <- "YES"
fin.preds$c309 <- as.factor(fin.preds$c309)

fin.preds$c318 <- ifelse(fin.preds$c318>=88,NA,fin.preds$c318)
fin.preds$c318 <- as.numeric(fin.preds$c318)

fin.preds$c319 <- ifelse(fin.preds$c319>=88,NA,fin.preds$c319)
fin.preds$c319 <- as.numeric(fin.preds$c319)

fin.preds$c320 <- ifelse(fin.preds$c320>=88,NA,fin.preds$c320)
fin.preds$c320 <- as.numeric(fin.preds$c320)

fin.preds$c321 <- ifelse(fin.preds$c321>=88,NA,fin.preds$c321)
fin.preds$c321 <- as.numeric(fin.preds$c321)

fin.preds$painkill <- as.character(fin.preds$painkill)
fin.preds$painkill[fin.preds$painkill=="8" | fin.preds$painkill=="9"] <- NA
fin.preds$painkill[fin.preds$painkill=="1"] <- "YES"
fin.preds$painkill[fin.preds$painkill=="2"] <- "NO"
fin.preds$painkill <- as.factor(fin.preds$painkill)


droppers <- c("c245","c260","c268","c293","c294")

fin.preds <- fin.preds[,-c(which(colnames(fin.preds) %in% droppers))]

class.order <- unlist(lapply(fin.preds,class))
numeric.columns <- which(class.order == "numeric")
factor.columns <- which(class.order != "numeric")

fin.preds_num <- fin.preds[,numeric.columns]
fin.preds_cat <- fin.preds[,factor.columns]
fin.preds_num.be <- escofier.transform(fin.preds_num)

fin.preds_num.be <- mean.impute(fin.preds_num.be)
fin.dat <- cbind(fin.preds_num.be,makeNominalData(fin.preds_cat))




ca.res <- epCA(fin.dat,graphs=F)
chi.mahal <- cbind(rowSums(ca.res$ExPosition.Data$fi^2),rowSums(ca.res$ExPosition.Data$pdq$p^2))

these.outliers <- which(chi.mahal[,1] > 3 | chi.mahal[,2] > 250)
these.inliers <- which(!(chi.mahal[,1] > 3 | chi.mahal[,2] > 250))

fin.preds_num.inliers <- fin.preds_num[these.inliers,]
fin.preds_cat.inliers <- fin.preds_cat[these.inliers,]
fin.preds_num.inliners.be <- escofier.transform(fin.preds_num.inliers)
fin.preds_num.inliners.be <- mean.impute(fin.preds_num.inliners.be)
fin.dat.inliers <- cbind(fin.preds_num.inliners.be,makeNominalData(fin.preds_cat.inliers))


fin.outs$askhelp <- ifelse(fin.outs$askhelp >= 8, NA, fin.outs$askhelp)
fin.outs$askhelp[fin.outs$askhelp==0] <- "NEVER"
fin.outs$askhelp[fin.outs$askhelp==1] <- "RARELY"
fin.outs$askhelp[fin.outs$askhelp==2] <- "SOMETIMES"
fin.outs$askhelp[fin.outs$askhelp==3] <- "FREQUENT"
fin.outs$askhelp[fin.outs$askhelp==4] <- "ALWAYS"
fin.outs$askhelp <- as.factor(fin.outs$askhelp)

fin.outs$notime <- ifelse(fin.outs$notime >= 8, NA, fin.outs$notime)
fin.outs$notime[fin.outs$notime==0] <- "NEVER"
fin.outs$notime[fin.outs$notime==1] <- "RARELY"
fin.outs$notime[fin.outs$notime==2] <- "SOMETIMES"
fin.outs$notime[fin.outs$notime==3] <- "FREQUENT"
fin.outs$notime[fin.outs$notime==4] <- "ALWAYS"
fin.outs$notime <- as.factor(fin.outs$notime)

fin.outs$feelstre <- ifelse(fin.outs$feelstre >= 8, NA, fin.outs$feelstre)
fin.outs$feelstre[fin.outs$feelstre==0] <- "NEVER"
fin.outs$feelstre[fin.outs$feelstre==1] <- "RARELY"
fin.outs$feelstre[fin.outs$feelstre==2] <- "SOMETIMES"
fin.outs$feelstre[fin.outs$feelstre==3] <- "FREQUENT"
fin.outs$feelstre[fin.outs$feelstre==4] <- "ALWAYS"
fin.outs$feelstre <- as.factor(fin.outs$feelstre)

fin.outs$feelemba <- ifelse(fin.outs$feelemba >= 8, NA, fin.outs$feelemba)
fin.outs$feelemba[fin.outs$feelemba==0] <- "NEVER"
fin.outs$feelemba[fin.outs$feelemba==1] <- "RARELY"
fin.outs$feelemba[fin.outs$feelemba==2] <- "SOMETIMES"
fin.outs$feelemba[fin.outs$feelemba==3] <- "FREQUENT"
fin.outs$feelemba[fin.outs$feelemba==4] <- "ALWAYS"
fin.outs$feelemba <- as.factor(fin.outs$feelemba)

fin.outs$feelangr <- ifelse(fin.outs$feelangr >= 8, NA, fin.outs$feelangr)
fin.outs$feelangr[fin.outs$feelangr==0] <- "NEVER"
fin.outs$feelangr[fin.outs$feelangr==1] <- "RARELY"
fin.outs$feelangr[fin.outs$feelangr==2] <- "SOMETIMES"
fin.outs$feelangr[fin.outs$feelangr==3] <- "FREQUENT"
fin.outs$feelangr[fin.outs$feelangr==4] <- "ALWAYS"
fin.outs$feelangr <- as.factor(fin.outs$feelangr)

fin.outs$affects <- ifelse(fin.outs$affects >= 8, NA, fin.outs$affects)
fin.outs$affects[fin.outs$affects==0] <- "NEVER"
fin.outs$affects[fin.outs$affects==1] <- "RARELY"
fin.outs$affects[fin.outs$affects==2] <- "SOMETIMES"
fin.outs$affects[fin.outs$affects==3] <- "FREQUENT"
fin.outs$affects[fin.outs$affects==4] <- "ALWAYS"
fin.outs$affects <- as.factor(fin.outs$affects)

fin.outs$afraid <- ifelse(fin.outs$afraid >= 8, NA, fin.outs$afraid)
fin.outs$afraid[fin.outs$afraid==0] <- "NEVER"
fin.outs$afraid[fin.outs$afraid==1] <- "RARELY"
fin.outs$afraid[fin.outs$afraid==2] <- "SOMETIMES"
fin.outs$afraid[fin.outs$afraid==3] <- "FREQUENT"
fin.outs$afraid[fin.outs$afraid==4] <- "ALWAYS"
fin.outs$afraid <- as.factor(fin.outs$afraid)

fin.outs$dependen <- ifelse(fin.outs$dependen >= 8, NA, fin.outs$dependen)
fin.outs$dependen[fin.outs$dependen==0] <- "NEVER"
fin.outs$dependen[fin.outs$dependen==1] <- "RARELY"
fin.outs$dependen[fin.outs$dependen==2] <- "SOMETIMES"
fin.outs$dependen[fin.outs$dependen==3] <- "FREQUENT"
fin.outs$dependen[fin.outs$dependen==4] <- "ALWAYS"
fin.outs$dependen <- as.factor(fin.outs$dependen)

fin.outs$strained <- ifelse(fin.outs$strained >= 8, NA, fin.outs$strained)
fin.outs$strained[fin.outs$strained==0] <- "NEVER"
fin.outs$strained[fin.outs$strained==1] <- "RARELY"
fin.outs$strained[fin.outs$strained==2] <- "SOMETIMES"
fin.outs$strained[fin.outs$strained==3] <- "FREQUENT"
fin.outs$strained[fin.outs$strained==4] <- "ALWAYS"
fin.outs$strained <- as.factor(fin.outs$strained)

fin.outs$suffered <- ifelse(fin.outs$suffered >= 8, NA, fin.outs$suffered)
fin.outs$suffered[fin.outs$suffered==0] <- "NEVER"
fin.outs$suffered[fin.outs$suffered==1] <- "RARELY"
fin.outs$suffered[fin.outs$suffered==2] <- "SOMETIMES"
fin.outs$suffered[fin.outs$suffered==3] <- "FREQUENT"
fin.outs$suffered[fin.outs$suffered==4] <- "ALWAYS"
fin.outs$suffered <- as.factor(fin.outs$suffered)

fin.outs$privacy <- ifelse(fin.outs$privacy >= 8, NA, fin.outs$privacy)
fin.outs$privacy[fin.outs$privacy==0] <- "NEVER"
fin.outs$privacy[fin.outs$privacy==1] <- "RARELY"
fin.outs$privacy[fin.outs$privacy==2] <- "SOMETIMES"
fin.outs$privacy[fin.outs$privacy==3] <- "FREQUENT"
fin.outs$privacy[fin.outs$privacy==4] <- "ALWAYS"
fin.outs$privacy <- as.factor(fin.outs$privacy)

fin.outs$soclife <- ifelse(fin.outs$soclife >= 8, NA, fin.outs$soclife)
fin.outs$soclife[fin.outs$soclife==0] <- "NEVER"
fin.outs$soclife[fin.outs$soclife==1] <- "RARELY"
fin.outs$soclife[fin.outs$soclife==2] <- "SOMETIMES"
fin.outs$soclife[fin.outs$soclife==3] <- "FREQUENT"
fin.outs$soclife[fin.outs$soclife==4] <- "ALWAYS"
fin.outs$soclife <- as.factor(fin.outs$soclife)

fin.outs$friends <- ifelse(fin.outs$friends >= 8, NA, fin.outs$friends)
fin.outs$friends[fin.outs$friends==0] <- "NEVER"
fin.outs$friends[fin.outs$friends==1] <- "RARELY"
fin.outs$friends[fin.outs$friends==2] <- "SOMETIMES"
fin.outs$friends[fin.outs$friends==3] <- "FREQUENT"
fin.outs$friends[fin.outs$friends==4] <- "ALWAYS"
fin.outs$friends <- as.factor(fin.outs$friends)

fin.outs$expect <- ifelse(fin.outs$expect >= 8, NA, fin.outs$expect)
fin.outs$expect[fin.outs$expect==0] <- "NEVER"
fin.outs$expect[fin.outs$expect==1] <- "RARELY"
fin.outs$expect[fin.outs$expect==2] <- "SOMETIMES"
fin.outs$expect[fin.outs$expect==3] <- "FREQUENT"
fin.outs$expect[fin.outs$expect==4] <- "ALWAYS"
fin.outs$expect <- as.factor(fin.outs$expect)

fin.outs$expenses <- ifelse(fin.outs$expenses >= 8, NA, fin.outs$expenses)
fin.outs$expenses[fin.outs$expenses==0] <- "NEVER"
fin.outs$expenses[fin.outs$expenses==1] <- "RARELY"
fin.outs$expenses[fin.outs$expenses==2] <- "SOMETIMES"
fin.outs$expenses[fin.outs$expenses==3] <- "FREQUENT"
fin.outs$expenses[fin.outs$expenses==4] <- "ALWAYS"
fin.outs$expenses <- as.factor(fin.outs$expenses)

fin.outs$unable <- ifelse(fin.outs$unable >= 8, NA, fin.outs$unable)
fin.outs$unable[fin.outs$unable==0] <- "NEVER"
fin.outs$unable[fin.outs$unable==1] <- "RARELY"
fin.outs$unable[fin.outs$unable==2] <- "SOMETIMES"
fin.outs$unable[fin.outs$unable==3] <- "FREQUENT"
fin.outs$unable[fin.outs$unable==4] <- "ALWAYS"
fin.outs$unable <- as.factor(fin.outs$unable)

fin.outs$lostctrl <- ifelse(fin.outs$lostctrl >= 8, NA, fin.outs$lostctrl)
fin.outs$lostctrl[fin.outs$lostctrl==0] <- "NEVER"
fin.outs$lostctrl[fin.outs$lostctrl==1] <- "RARELY"
fin.outs$lostctrl[fin.outs$lostctrl==2] <- "SOMETIMES"
fin.outs$lostctrl[fin.outs$lostctrl==3] <- "FREQUENT"
fin.outs$lostctrl[fin.outs$lostctrl==4] <- "ALWAYS"
fin.outs$lostctrl <- as.factor(fin.outs$lostctrl)

fin.outs$leave <- ifelse(fin.outs$leave >= 8, NA, fin.outs$leave)
fin.outs$leave[fin.outs$leave==0] <- "NEVER"
fin.outs$leave[fin.outs$leave==1] <- "RARELY"
fin.outs$leave[fin.outs$leave==2] <- "SOMETIMES"
fin.outs$leave[fin.outs$leave==3] <- "FREQUENT"
fin.outs$leave[fin.outs$leave==4] <- "ALWAYS"
fin.outs$leave <- as.factor(fin.outs$leave)

fin.outs$uncertai <- ifelse(fin.outs$uncertai >= 8, NA, fin.outs$uncertai)
fin.outs$uncertai[fin.outs$uncertai==0] <- "NEVER"
fin.outs$uncertai[fin.outs$uncertai==1] <- "RARELY"
fin.outs$uncertai[fin.outs$uncertai==2] <- "SOMETIMES"
fin.outs$uncertai[fin.outs$uncertai==3] <- "FREQUENT"
fin.outs$uncertai[fin.outs$uncertai==4] <- "ALWAYS"
fin.outs$uncertai <- as.factor(fin.outs$uncertai)

fin.outs$doing <- ifelse(fin.outs$doing >= 8, NA, fin.outs$doing)
fin.outs$doing[fin.outs$doing==0] <- "NEVER"
fin.outs$doing[fin.outs$doing==1] <- "RARELY"
fin.outs$doing[fin.outs$doing==2] <- "SOMETIMES"
fin.outs$doing[fin.outs$doing==3] <- "FREQUENT"
fin.outs$doing[fin.outs$doing==4] <- "ALWAYS"
fin.outs$doing <- as.factor(fin.outs$doing)

fin.outs$better <- ifelse(fin.outs$better >= 8, NA, fin.outs$better)
fin.outs$better[fin.outs$better==0] <- "NEVER"
fin.outs$better[fin.outs$better==1] <- "RARELY"
fin.outs$better[fin.outs$better==2] <- "SOMETIMES"
fin.outs$better[fin.outs$better==3] <- "FREQUENT"
fin.outs$better[fin.outs$better==4] <- "ALWAYS"
fin.outs$better <- as.factor(fin.outs$better)

fin.outs$burdened <- ifelse(fin.outs$burdened >= 8, NA, fin.outs$burdened)
fin.outs$burdened[fin.outs$burdened==0] <- "NEVER"
fin.outs$burdened[fin.outs$burdened==1] <- "RARELY"
fin.outs$burdened[fin.outs$burdened==2] <- "SOMETIMES"
fin.outs$burdened[fin.outs$burdened==3] <- "FREQUENT"
fin.outs$burdened[fin.outs$burdened==4] <- "ALWAYS"
fin.outs$burdened <- as.factor(fin.outs$burdened)

fin.outs$zarscore <- ifelse(fin.outs$zarscore >= 99, NA, fin.outs$zarscore)
fin.outs$zarscore <- as.numeric(fin.outs$zarscore)

fin.outs$health1 <- ifelse(fin.outs$health1 >= 7, NA, fin.outs$health1)
fin.outs$health1[fin.outs$health1==1] <- "VERY.GOOD"
fin.outs$health1[fin.outs$health1==2] <- "PRETTY.GOOD"
fin.outs$health1[fin.outs$health1==3] <- "NOT.TOO.GOOD"
fin.outs$health1[fin.outs$health1==4] <- "POOR"
fin.outs$health1[fin.outs$health1==5] <- "VERY.POOR"
fin.outs$health1 <- as.factor(fin.outs$health1)

fin.outs$health2 <- ifelse(fin.outs$health2 >= 7, NA, fin.outs$health2)
fin.outs$health2[fin.outs$health2==1] <- "BETTER"
fin.outs$health2[fin.outs$health2==2] <- "ABOUT.SAME"
fin.outs$health2[fin.outs$health2==3] <- "WORSE"
fin.outs$health2 <- as.factor(fin.outs$health2)

fin.outs$bother <- ifelse(fin.outs$bother >= 8, NA, fin.outs$bother)
fin.outs$bother[fin.outs$bother==0] <- "<1"
fin.outs$bother[fin.outs$bother==1] <- "1_2"
fin.outs$bother[fin.outs$bother==2] <- "3_4"
fin.outs$bother[fin.outs$bother==3] <- "5_7"
fin.outs$bother <- as.factor(fin.outs$bother)

fin.outs$poorapp <- ifelse(fin.outs$poorapp >= 8, NA, fin.outs$poorapp)
fin.outs$poorapp[fin.outs$poorapp==0] <- "<1"
fin.outs$poorapp[fin.outs$poorapp==1] <- "1_2"
fin.outs$poorapp[fin.outs$poorapp==2] <- "3_4"
fin.outs$poorapp[fin.outs$poorapp==3] <- "5_7"
fin.outs$poorapp <- as.factor(fin.outs$poorapp)

fin.outs$shake <- ifelse(fin.outs$shake >= 8, NA, fin.outs$shake)
fin.outs$shake[fin.outs$shake==0] <- "<1"
fin.outs$shake[fin.outs$shake==1] <- "1_2"
fin.outs$shake[fin.outs$shake==2] <- "3_4"
fin.outs$shake[fin.outs$shake==3] <- "5_7"
fin.outs$shake <- as.factor(fin.outs$shake)

fin.outs$good <- ifelse(fin.outs$good >= 8, NA, fin.outs$good)
fin.outs$good[fin.outs$good==0] <- "<1"
fin.outs$good[fin.outs$good==1] <- "1_2"
fin.outs$good[fin.outs$good==2] <- "3_4"
fin.outs$good[fin.outs$good==3] <- "5_7"
fin.outs$good <- as.factor(fin.outs$good)

fin.outs$mind <- ifelse(fin.outs$mind >= 8, NA, fin.outs$mind)
fin.outs$mind[fin.outs$mind==0] <- "<1"
fin.outs$mind[fin.outs$mind==1] <- "1_2"
fin.outs$mind[fin.outs$mind==2] <- "3_4"
fin.outs$mind[fin.outs$mind==3] <- "5_7"
fin.outs$mind <- as.factor(fin.outs$mind)

fin.outs$depressd <- ifelse(fin.outs$depressd >= 8, NA, fin.outs$depressd)
fin.outs$depressd[fin.outs$depressd==0] <- "<1"
fin.outs$depressd[fin.outs$depressd==1] <- "1_2"
fin.outs$depressd[fin.outs$depressd==2] <- "3_4"
fin.outs$depressd[fin.outs$depressd==3] <- "5_7"
fin.outs$depressd <- as.factor(fin.outs$depressd)

fin.outs$eveffort <- ifelse(fin.outs$eveffort >= 8, NA, fin.outs$eveffort)
fin.outs$eveffort[fin.outs$eveffort==0] <- "<1"
fin.outs$eveffort[fin.outs$eveffort==1] <- "1_2"
fin.outs$eveffort[fin.outs$eveffort==2] <- "3_4"
fin.outs$eveffort[fin.outs$eveffort==3] <- "5_7"
fin.outs$eveffort <- as.factor(fin.outs$eveffort)

fin.outs$hopeful <- ifelse(fin.outs$hopeful >= 8, NA, fin.outs$hopeful)
fin.outs$hopeful[fin.outs$hopeful==0] <- "<1"
fin.outs$hopeful[fin.outs$hopeful==1] <- "1_2"
fin.outs$hopeful[fin.outs$hopeful==2] <- "3_4"
fin.outs$hopeful[fin.outs$hopeful==3] <- "5_7"
fin.outs$hopeful <- as.factor(fin.outs$hopeful)

fin.outs$failure <- ifelse(fin.outs$failure >= 8, NA, fin.outs$failure)
fin.outs$failure[fin.outs$failure==0] <- "<1"
fin.outs$failure[fin.outs$failure==1] <- "1_2"
fin.outs$failure[fin.outs$failure==2] <- "3_4"
fin.outs$failure[fin.outs$failure==3] <- "5_7"
fin.outs$failure <- as.factor(fin.outs$failure)

fin.outs$fearful <- ifelse(fin.outs$fearful >= 8, NA, fin.outs$fearful)
fin.outs$fearful[fin.outs$fearful==0] <- "<1"
fin.outs$fearful[fin.outs$fearful==1] <- "1_2"
fin.outs$fearful[fin.outs$fearful==2] <- "3_4"
fin.outs$fearful[fin.outs$fearful==3] <- "5_7"
fin.outs$fearful <- as.factor(fin.outs$fearful)

fin.outs$restls <- ifelse(fin.outs$restls >= 8, NA, fin.outs$restls)
fin.outs$restls[fin.outs$restls==0] <- "<1"
fin.outs$restls[fin.outs$restls==1] <- "1_2"
fin.outs$restls[fin.outs$restls==2] <- "3_4"
fin.outs$restls[fin.outs$restls==3] <- "5_7"
fin.outs$restls <- as.factor(fin.outs$restls)

fin.outs$happy <- ifelse(fin.outs$happy >= 8, NA, fin.outs$happy)
fin.outs$happy[fin.outs$happy==0] <- "<1"
fin.outs$happy[fin.outs$happy==1] <- "1_2"
fin.outs$happy[fin.outs$happy==2] <- "3_4"
fin.outs$happy[fin.outs$happy==3] <- "5_7"
fin.outs$happy <- as.factor(fin.outs$happy)

fin.outs$talkless <- ifelse(fin.outs$talkless >= 8, NA, fin.outs$talkless)
fin.outs$talkless[fin.outs$talkless==0] <- "<1"
fin.outs$talkless[fin.outs$talkless==1] <- "1_2"
fin.outs$talkless[fin.outs$talkless==2] <- "3_4"
fin.outs$talkless[fin.outs$talkless==3] <- "5_7"
fin.outs$talkless <- as.factor(fin.outs$talkless)

fin.outs$lonely <- ifelse(fin.outs$lonely >= 8, NA, fin.outs$lonely)
fin.outs$lonely[fin.outs$lonely==0] <- "<1"
fin.outs$lonely[fin.outs$lonely==1] <- "1_2"
fin.outs$lonely[fin.outs$lonely==2] <- "3_4"
fin.outs$lonely[fin.outs$lonely==3] <- "5_7"
fin.outs$lonely <- as.factor(fin.outs$lonely)

fin.outs$unfriend <- ifelse(fin.outs$unfriend >= 8, NA, fin.outs$unfriend)
fin.outs$unfriend[fin.outs$unfriend==0] <- "<1"
fin.outs$unfriend[fin.outs$unfriend==1] <- "1_2"
fin.outs$unfriend[fin.outs$unfriend==2] <- "3_4"
fin.outs$unfriend[fin.outs$unfriend==3] <- "5_7"
fin.outs$unfriend <- as.factor(fin.outs$unfriend)

fin.outs$enjoylif <- ifelse(fin.outs$enjoylif >= 8, NA, fin.outs$enjoylif)
fin.outs$enjoylif[fin.outs$enjoylif==0] <- "<1"
fin.outs$enjoylif[fin.outs$enjoylif==1] <- "1_2"
fin.outs$enjoylif[fin.outs$enjoylif==2] <- "3_4"
fin.outs$enjoylif[fin.outs$enjoylif==3] <- "5_7"
fin.outs$enjoylif <- as.factor(fin.outs$enjoylif)

fin.outs$cryspell <- ifelse(fin.outs$cryspell >= 8, NA, fin.outs$cryspell)
fin.outs$cryspell[fin.outs$cryspell==0] <- "<1"
fin.outs$cryspell[fin.outs$cryspell==1] <- "1_2"
fin.outs$cryspell[fin.outs$cryspell==2] <- "3_4"
fin.outs$cryspell[fin.outs$cryspell==3] <- "5_7"
fin.outs$cryspell <- as.factor(fin.outs$cryspell)

fin.outs$feltsad <- ifelse(fin.outs$feltsad >= 8, NA, fin.outs$feltsad)
fin.outs$feltsad[fin.outs$feltsad==0] <- "<1"
fin.outs$feltsad[fin.outs$feltsad==1] <- "1_2"
fin.outs$feltsad[fin.outs$feltsad==2] <- "3_4"
fin.outs$feltsad[fin.outs$feltsad==3] <- "5_7"
fin.outs$feltsad <- as.factor(fin.outs$feltsad)

fin.outs$dislike <- ifelse(fin.outs$dislike >= 8, NA, fin.outs$dislike)
fin.outs$dislike[fin.outs$dislike==0] <- "<1"
fin.outs$dislike[fin.outs$dislike==1] <- "1_2"
fin.outs$dislike[fin.outs$dislike==2] <- "3_4"
fin.outs$dislike[fin.outs$dislike==3] <- "5_7"
fin.outs$dislike <- as.factor(fin.outs$dislike)

fin.outs$getgoing <- ifelse(fin.outs$getgoing >= 8, NA, fin.outs$getgoing)
fin.outs$getgoing[fin.outs$getgoing==0] <- "<1"
fin.outs$getgoing[fin.outs$getgoing==1] <- "1_2"
fin.outs$getgoing[fin.outs$getgoing==2] <- "3_4"
fin.outs$getgoing[fin.outs$getgoing==3] <- "5_7"
fin.outs$getgoing <- as.factor(fin.outs$getgoing)

fin.outs$cesscore <- ifelse(fin.outs$cesscore >= 99, NA, fin.outs$cesscore)
fin.outs$cesscore <- as.numeric(fin.outs$cesscore)


class.order.out <- unlist(lapply(fin.outs,class))
numeric.columns.out <- which(class.order.out == "numeric")
factor.columns.out <- which(class.order.out != "numeric")


fin.outs_num <- fin.outs[rownames(fin.dat.inliers),numeric.columns.out]
fin.outs_cat <- fin.outs[rownames(fin.dat.inliers),factor.columns.out]

fin.outs_num.be <- escofier.transform(fin.outs_num)
fin.outs_num.be <- mean.impute(fin.outs_num.be)
fin.dat.outs <- cbind(fin.outs_num.be,makeNominalData(fin.outs_cat))



ca.res_outs <- epCA(fin.dat.outs,graphs=F)
chi.mahal_outs <- cbind(rowSums(ca.res_outs$ExPosition.Data$fi^2),rowSums(ca.res_outs$ExPosition.Data$pdq$p^2))

these.outliers_outs <- which(chi.mahal_outs[,1] > 11 | chi.mahal_outs[,2] > 400)
these.inliers_outs <- which(!(chi.mahal_outs[,1] > 11 | chi.mahal_outs[,2] > 400))
# 
fin.outs_num.inliers <- fin.outs_num[these.inliers_outs,]
fin.outs_cat.inliers <- fin.outs_cat[these.inliers_outs,]
fin.outs_num.inliners.be <- escofier.transform(fin.outs_num.inliers)
fin.outs_num.inliners.be <- mean.impute(fin.outs_num.inliners.be)
fin.dat.inliers_outs <- cbind(fin.outs_num.inliners.be,makeNominalData(fin.outs_cat.inliers))

final.set.obs <- intersect(rownames(fin.dat.inliers_outs),rownames(fin.dat.inliers))
final.preds <- fin.dat.inliers[final.set.obs,]
final.outs <- fin.dat.inliers_outs[final.set.obs,]


pred.ca <- epCA(final.preds,graphs=F)
outs.ca <- epCA(final.outs,graphs=F)
plsca.res <- tepPLSCA(DATA1 = final.preds,DATA2 = final.outs, make_data1_nominal = F,make_data2_nominal = F,graphs=F)


boot.iters <- 1000
fi.boot <- array(NA,dim=c(nrow(plsca.res$TExPosition.Data$fi),length(plsca.res$TExPosition.Data$eigs),boot.iters))
rownames(fi.boot) <- rownames(plsca.res$TExPosition.Data$fi)
fj.boot <- array(NA,dim=c(nrow(plsca.res$TExPosition.Data$fj),length(plsca.res$TExPosition.Data$eigs),boot.iters))
rownames(fj.boot) <- rownames(plsca.res$TExPosition.Data$fj)

FIN.DX.DES <- makeNominalData(as.matrix(fin.des[rownames(final.preds),"DEMENTIA"]))	
for(i in 1:boot.iters){
  boot.sample <- boot.samples(final.preds,FIN.DX.DES,T)
  DAT1.boot <- final.preds[boot.sample,]
  DAT2.boot <- final.outs[boot.sample,]  
  
  boot.R <- t(DAT1.boot) %*% DAT2.boot
  fi.boot[,,i] <- (rowNorms(boot.R,type="ca") %*% plsca.res$TExPosition.Data$fj %*% diag(1/plsca.res$TExPosition.Data$pdq$Dv))
  fi.boot[is.na(fi.boot)] <- 0
  fj.boot[,,i] <- rowNorms(t(boot.R),type="ca") %*% plsca.res$TExPosition.Data$fi %*% diag(1/plsca.res$TExPosition.Data$pdq$Dv)
  fj.boot[is.na(fj.boot)] <- 0
  
}

fi.bsrs <- boot.ratio.test(fi.boot,critical.value = 4)
fj.bsrs <- boot.ratio.test(fj.boot,critical.value = 4)	## stability of each point; but because of dual coding this is incorrect.


save.image('InferenceOut.RData')