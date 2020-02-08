library(genpwr)

genpwr.calc(calc="power",
            model="linear",
            ge.interaction="continuous",
            N=300000,
            MAF=0.25,
            Alpha=5e-8,
            sd_y=1,
            sd_e=1,
            ES_G=sqrt(0.005),
            ES_E=sqrt(0.01),
            ES_GE=sqrt(c(0.0001, 0.0005, 0.001, 0.005, 0.01)),
            True.Model="Additive",
            Test.Model="Additive")

genpwr.calc(calc="power",
            model="linear",
            ge.interaction="continuous",
            N=300000,
            MAF=0.25,
            Alpha=5e-8,
            sd_y=1,
            sd_e=1,
            ES_G=sqrt(0.005),
            ES_E=sqrt(0.01),
            ES_GE=sqrt(c(0.0001, 0.0002, 0.0003, 0.00035)),
            True.Model="Additive",
            Test.Model="Additive")

genpwr.calc(calc="power",
            model="linear",
            ge.interaction="continuous",
            N=10000,
            MAF=0.25,
            Alpha=5e-8,
            sd_y=1,
            sd_e=1,
            ES_G=sqrt(0.005),
            ES_E=sqrt(0.01),
            ES_GE=sqrt(c(0.01)),
            True.Model="Additive",
            Test.Model="Additive")

# genpwr.calc(calc = "power", model = "logistic", 
#             ge.interaction = "continuous",
#             N=500, Case.Rate=0.3, MAF=seq(0.2,0.4,0.02), OR_G=3, 
#             OR_E=1, OR_GE=4, 
#             Alpha=0.05, True.Model='All', Test.Model='All')
