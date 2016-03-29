library(shiny)
library(UsingR)
library(plotrix)

find_cross_root <- function (mean1, sd1, mean2, sd2, x) {

    y1 <- function(x) ( dnorm(x, mean = mean1, sd = sd1) )
    y2 <- function(x) ( dnorm(x, mean = mean2, sd = sd2) )
    g <- function(x) y1(x) - y2(x);

    xx = uniroot(g, c(min(x),max(x)) );

    return(xx$root);
}

calc_err_rates <- function(mean1, sd1, mean2, sd2, threshold, x) {
    result = list();

    xmin = min(x);
    xmax = max(x);

    xcross = find_cross_root(mean1, sd1, mean2, sd2, x);

    y1 <- function(x) ( dnorm(x, mean = mean1, sd = sd1) )
    y2 <- function(x) ( dnorm(x, mean = mean2, sd = sd2) )

    tp = integrate(y2, threshold, mean2 + 4*sd2)$value;
    fp = integrate(y1, threshold, mean1 + 4*sd1)$value;
    tn = integrate(y1, mean1 - 4*sd1, threshold)$value;
    fn = integrate(y2, mean2 - 4*sd2, threshold)$value;


    if (threshold > xcross) {
        tp1 = integrate(y2, threshold, mean2 + sd2*4)$value - fp;
        tn1 = integrate(y1, mean1 - sd1*4, xcross)$value - integrate(y2, mean2 - sd2*4, xcross)$value;
    } else if (threshold < xcross) {
        tp1 = integrate(y2, xcross, mean2 + sd2*4)$value - integrate(y1, xcross, mean1 + sd1*4)$value;
        tn1 = integrate(y1, mean1 - sd1*4, threshold)$value - fn;
    } else if (threshold == xcross) {
        tp1 = integrate(y2, threshold, mean2 + sd2*4)$value - fp;
        tn1 = integrate(y1, mean1 - sd1*4, threshold)$value - fn;
    }

    result[['TP']] = round(tp, 3);
    result[['TN']] = round(tn, 3);
    result[['FP']] = round(fp, 3);
    result[['FN']] = round(fn, 3);

    return(result);
}

calc_rates <- function(TP, TN, FP, FN, total) {
    result = matrix();

    TPR = round( TP / (TP + FN) , 4); # true positive rate, sensitivity
    TNR = round( TN / (FP + TN) , 4);; # true negative rate, specificity

    PPV = round( TP / (TP + FP) , 4);; # positive predictive value, precision
    NPV = round( TN / (TN + FN) , 4);; # negative predictive value

    FPR = round( FP / (FP + TN) , 4);; # false positive rate, fall-out
    FNR = round( FN / (FN + TP) , 4);; # false negative rate, miss rate

    FDR = round( FP / (FP + TP) , 4);; # false discovery rate

    ACC = round( (TP + TN) / (TP + FN + FP + TN) , 4);; # accuracy

    result <- matrix(c(TPR, TNR, PPV, NPV, FPR, FNR, FDR, ACC), nrow = 8, byrow = TRUE);
    rownames(result) <- c(
        'Sensitivity (Recall, True Positive Rate)',
        'Specificity (True Negative Rate)',
        'Precision (Positive Predictive Value)',
        'Negative Predictive Value',
        'Fall-out (False Positive Rate)',
        'Miss Rate (False Negative Rate)',
        'False Discovery Rate',
        'Accuracy' );
    colnames(result) <- 'value';

    return(result);
}

shinyServer(
	function(input, output) {

    # constants used further
	STEP = 0.01;
	num_pred = 5000;
    tp_col = 'pink';
    tn_col = 'cyan';
    fp_col = 'brown1';
    fn_col = 'aquamarine1';

    inpData <- reactive({
        input$Submit
        mean1 	<- input$mean1;
        mean2 	<- input$mean2;
        sd1 	<- input$sd1;
        sd2 	<- input$sd2;
        thr 	<- input$thr;
        return(c(mean1, mean2, sd1, sd2, thr));
    })

    calcUtilRates <- reactive({
      tmp = inpData();
      x = seq(tmp[1] - 4*tmp[3], tmp[2] + 4*tmp[4], by = 0.01);
      err_rates = calc_err_rates(tmp[1], tmp[3], tmp[2], tmp[4], tmp[5], x);
      util_rates = calc_rates(err_rates$TP, err_rates$TN, err_rates$FP, err_rates$FN, 1);
    })

		output$tpr_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[1]
    })

		output$tnr_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[2]
    })

		output$ppv_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[3]
    })

		output$npv_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[4]
    })

		output$fpr_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[5]
    })

		output$fnr_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[6]
    })

		output$fdr_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[7]
    })

		output$acc_out <- renderPrint({
      util_rates = calcUtilRates();
      util_rates[8]
    })

	output$le_plot <- renderPlot(
	{
	    le_data = inpData();
        mean1 = le_data[1];
        mean2 = le_data[2];
        std1 = le_data[3];
        std2 = le_data[4];
        THRESHOLD = le_data[5];

        THR_X = THRESHOLD;

        X_MIN = mean1 - 4 * std1;
        X_MAX = mean2 + 4 * std2;

        x_min1 = X_MIN;
        x_max1 = mean1 + 4 * std1;

        x_min2 = mean2 - 4 * std2;
        x_max2 = X_MAX;

        # Assume that Density1 represents null-hypothesis
        t2_err_x_min = x_min2;
        t2_err_x_max = THR_X;

        t1_err_x_min = THR_X;
        t1_err_x_max = x_max1;

        x1 = seq(x_min1, x_max1, by = STEP);
        y1 = dnorm(x1, mean = mean1, sd = std1);

        x2 = seq(x_min2, x_max2, by = STEP);
        y2 = dnorm(x2, mean = mean2, sd = std2);

        x_full_range = seq(X_MIN, X_MAX, by = STEP);

        # To make a polygon grow from X-axis upwards, we need to add additional boardering values to our x/y sets
        t1_err_x = seq(t1_err_x_min, t1_err_x_max, by = STEP);
        t1_err_y = c(0, dnorm(t1_err_x, mean = mean1, sd = std1), 0);
        t1_err_x = c(THR_X, t1_err_x, x_max1);

        # To make a polygon grow from X-axis upwards, we need to add additional boardering values to our x/y sets
        t2_err_x = seq(t2_err_x_min, t2_err_x_max, by = STEP);
        t2_err_y = c(0, dnorm(t2_err_x, mean = mean2, sd = std2), 0);
        t2_err_x = c(x_min2, t2_err_x, THR_X);

        lim_xl = X_MIN * 1.1;
        lim_xr = X_MAX * 1.1;

        lim_yu = max(c(y1,y2)) * 1.1;
        lim_yd = -lim_yu*0.9;

        THR_Y = lim_yu * 1.2;

        err_rates = calc_err_rates(mean1, std1, mean2, std2, THR_X, x_full_range);
        tmp_tbl <- matrix(c(err_rates$TP * num_pred, err_rates$FP * num_pred, err_rates$FN * num_pred, err_rates$TN * num_pred),
          nrow = 2, byrow = TRUE);
        colnames(tmp_tbl) <- c('True +' , 'True -');
        rownames(tmp_tbl) <- c('Predicted +' , 'Predicted -');

        plot(x1, y1, pch = '.', xlim = c(lim_xl, lim_xr) , ylim = c(lim_yd, lim_yu), xlab = 'random variable value',
          ylab = 'probability density value');
        polygon(x1, y1, col = tn_col);
        polygon(x2, y2, col = tp_col);
        polygon(t1_err_x, t1_err_y, col = fp_col);
        polygon(t2_err_x, t2_err_y, col = fn_col);
        lines(c(THR_X, THR_X), c(0, THR_Y), col = 'purple', lty = 1, lwd = 2);
        #lines(c(cross_x, cross_x), c(0, THR_Y), col = 'black', lty = 3);
        legend("topright", c('True negative', 'True positive', 'False positive', 'False negative'),
          col = c(tn_col, tp_col, fp_col, fn_col), lty=c(1, 1, 1, 1), lwd=c(6, 6, 6, 6) );

        table_title1 = sprintf("Expected confusion matrix per %d predictions:", num_pred * 2);
        #table_title2 = sprintf("Utility ratios:")
        addtable2plot(0, -0.3, tmp_tbl, bty="o", display.rownames=TRUE, vlines = TRUE, hlines=TRUE, title=table_title1)
        #addtable2plot(-1.2, -0.9, util_rates, bty="o", display.rownames=TRUE, vlines = TRUE, hlines=TRUE, title=table_title2)
			}
		);
	}
)
