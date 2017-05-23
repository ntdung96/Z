GREwordassign <- function(seed = 100, pvalue = 0.05) {
      hardness <- c(6, 11, 11, 17, 21, 41, 51, 55, 66, 67,
            91, 109, 146, 204, 291, 303, 393, 494, 576, 928)
      for(i in 1:seed) {
            set.seed(i)
            group <- sample(1:2, size = 20, replace = TRUE)
            if(length(group[which(group == 1)]) == length(group[which(group == 2)])) {
                  p <- t.test(hardness~group)$p.value       ## p-value of ind t-test
                  if(p > pvalue) {
                        print(i)
                        print(group)
                        print(p)
                        break
                  }
            }
            if(i == seed) {
                  stop("no sequence found, increase seed numbers\n")
            }
      }
}
