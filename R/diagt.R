diagt <-
function (D, btest, conf.level=0.95, digits = 3) 
{
    if (is.null(digits)) {digits = getOption("digits")}
    table1 <- table(btest, D)
    chi2 <- chisq.test(table1)
    if (nrow(table1) == 1) {
        if (row.names(table1) == "TRUE") {
            table1 <- rbind(c(0, 0), table1)
        }
    }
    table2 <- table1[2:1, 2:1]
    n <- (table2[1, 1] + table2[1, 2] + table2[2, 2] + table2[2, 
        1])
    f.table2 <- as.data.frame(table2)
    f.table2 <- cbind(c("Test positive", "Test Negative"), f.table2)
    colnames(f.table2) <- c("Test Results", "True Disease", "True Non Disease")
    TP <- table2[1, 1]
    TN <- table2[2, 2]
    FP <- table2[1, 2]
    FN <- table2[2, 1]
    results <- c(Sensitivity = signif(TP/(TP + FN), digits = digits), 
        Specificity = signif( TN/(TN + FP), digits = digits), 
        PPV = signif(TP/(TP + FP), digits = digits), 
				NPV = signif(TN/(TN + FN), digits = digits), 
				Correctly.classified = signif((TN + TP)/(TN + FN + TP + FP), digits = digits))
    ci_results <- c(Sensitivity = cii((TP + FN), TP, conf.level=conf.level), 
        Specificity = cii((TN + FP), TN, conf.level =conf.level), 
				PPV = cii((TP + FP), TP, conf.level=conf.level), 
				NPV = cii((TN + FN), TN, conf.level=conf.level), 
        DA = cii(n, TP + TN, conf.level=conf.level))
    Accuracy <- c("Sensitivity", "Specificity", "Positive predictive value", 
        "Negative predictive.value", "Correctly classified")
    t1 <- data.frame(cbind(Accuracy, results, ci_results))
    cat("\nTotal number of case", n, "\n")
    print.char.matrix(f.table2, col.names = T, row.names = F)
    cat("\nSummary of test accuracy (p-value", formatC(chi2$p.value, 
        format = "f", digit = 4), ")\n")
    print.char.matrix(t1, col.names = T, row.names = F)
}

