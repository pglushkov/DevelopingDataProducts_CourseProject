library(shiny)
shinyUI(
	pageWithSidebar(
		headerPanel('Developing Data Products Course Project, student : Peter Glushkov'),

		sidebarPanel(
			h3('Description and short manual'),
			p('A topic for course project is simple, yet very important concept in statistics - selection between
			null-hypothesis (H0) and alternative hypothesis (H1). Examples of this process are numerous and can be
			found in dozens of technical applications. In this project we assume that user is familiar with the concept
			and provide as simple means to just visualize the process. Selection between two normal distributions, one
			of which is associated with H0 and the other one with H1, is assumed. Choise to accept or
			reject the H0 is done based on a simple threshold - if random variable value is bigger then the threshold -
			H0 is rejected. Corresponding Type-1 (false-positive) and Type-2 (false-negative) errors are vizualized
			along with distributions for better presentation of results. A set of associated utility characteristics are
			calculated and presented as well. In such manner, one can play around with certain parameters and see the
			results of it in an interactive manner.'),
			p('Short manual:'),
			p('- set H0-rejection threshold (aka detection threshold)'),
			p('- set mean and standart deviation values for H0 and H1'),
			p('- hit Submit button to update the results'),
			p('- result of simulation will be presented on the right panel of the web-page'),
			p('Unfortunately UI is not perfect and some unlucky selection of parameters may lead to numerical issues. If
			such situation is met - try to make sure that:'),
			p('- distributions DO overlap'),
			p('- detection threshold lies between mean values of H0 and H1'),
			p('- if nothing helps - restart the page, default parameters will be used on restart'),

			h3('Specify detection threshold'),
			#numericInput( 'thr', 'Detection Threshold', 2, min = -9, max = 19, step = 0.1 ),
			sliderInput('thr', 'Detection Threshold', value = 2, min = 0.5, max = 8.5, step = 0.05),

			h2(' '),
			h3('Specify parameters of distributions'),
			sliderInput( 'mean1', 'H0 mean', value = 0, min = 0, max = 4, step = 0.05 ),
			sliderInput( 'mean2', 'H1 mean', value = 4, min = 4, max = 9, step = 0.05 ),
			sliderInput( 'sd1', 'H0 standard Deviation', value = 1, min = 0.5, max = 5, step = 0.05 ),
			sliderInput( 'sd2', 'H1 Standard Deviation', value = 1, min = 0.5, max = 5, step = 0.05 ),

			submitButton('Submit'),

			p('   '),
			p('   '),
			p('In case any poor soul, God bless him, will have courage to look through this incredible mess that is the
			code of this project, the link to the repo is :'),
			p('https://github.com/pglushkov/DevelopingDataProducts_CourseProject')
		),

		mainPanel(
			h1('Results of simulation'),

			h2('1) Distributions and confusion matrix'),
			plotOutput('le_plot', height = '600px'),

			h2('2) Some utility rates'),
			h3('Sensitivity (Recall, True Positive Rate):'),
			verbatimTextOutput('tpr_out'),
			h3('Specificity (True Negative Rate):'),
			verbatimTextOutput('tnr_out'),
			h3('Precision (Positive Predictive Value):'),
			verbatimTextOutput('ppv_out'),
			h3('Negative Predictive Value:'),
			verbatimTextOutput('npv_out'),
			h3('Fall-out (False Positive Rate):'),
			verbatimTextOutput('fpr_out'),
			h3('Miss Rate (False Negative Rate):'),
			verbatimTextOutput('fnr_out'),
			h3('False Discovery Rate:'),
			verbatimTextOutput('fdr_out'),
			h3('Accuracy:'),
			verbatimTextOutput('acc_out')
		)
	)
)
