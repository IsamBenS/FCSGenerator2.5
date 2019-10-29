generate.pattern.codes <- function(nmb.pattern.codes, nmb.params)
{
    pattern.codes <- list()
    nmb.seuils <- 2
    for (i in 1:nmb.pattern.codes) 
    {
        tmp.code <- c()
        while(length(tmp.code)==0 || list(tmp.code) %in% pattern.codes)
        {
            tmp.code <- sapply(1:nmb.params, function(i)
            {
                return(sample(1:nmb.seuils, 1))
            })
        }
        pattern.codes[[i]] <- tmp.code
    }
    
    return(pattern.codes)
}

generate.position.from.pattern <- function(pattern.code, lower.value, upper.value, nmb.pos=2)
{
    l <- length(pattern.code)
    range.unit <- (upper.value - lower.value)/nmb.pos
    m.list <- sapply(1:l, function(i)
    {
        pop.mean <- pattern.code[i]
        pop.mean <- runif(1, (pop.mean-2/3)*range.unit, (pop.mean-1/2)*range.unit)
        return(pop.mean)
    })
    sd.list <- sapply(1:l, function(i)
    {
        return(range.unit/l/3)
    })
    
    return(list(m.list,sd.list))
}

generate.expression.matrix.from.position <- function(pop.number, position, nmb.events, lower.value = -0.5, upper.value = 4.5)
{
    pop.events <- as.integer(nmb.events)
    m.list <- position[[1]]
    sd.list <- position[[2]]
    nmb.dim <- length(m.list)
    
    
    exp.mat <- matrix(nrow = nmb.events, ncol = length(m.list))
    for(i in 1:nmb.dim) #generate a normal distribution for each marker / parameter
    {
        exp.mat[,i] <- rtruncnorm(nmb.events, lower.value, upper.value, m.list[[i]], sd.list[[i]])
    }
    exp.mat <- cbind(exp.mat, 0)
    exp.mat <- cbind(exp.mat, pop.number)
    
    
    colnames(exp.mat) <- c(rep("NA",times = nmb.dim),"time","Population")
    for (current_dim in 1:nmb.dim)
    {
        colnames(exp.mat)[current_dim] <- paste("PARAM_",current_dim, sep="")
    }
    
    # fcs <- flowFrame(exp.mat)
    # descR <- description(fcs)
    # lapply(c(1:dim(exp.mat)[2]),function(x)
    # {
    #     descR[[paste0("$P",x,"R")]] <<- 262144
    # })
    
    nmb.grp <- min(nmb.events,1000)
    # descR[["TIMESTEP"]] <- 1/nmb.grp
    for(e in 1:nmb.events)
    {
        exp.mat[e,nmb.dim+1] <- 1/nmb.grp *  as.integer(e / (nmb.events/nmb.grp))
    }
    
    return(exp.mat)
}