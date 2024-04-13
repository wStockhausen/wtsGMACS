p2e = "~/Work/StockAssessments-Crab/AssessmentModelDevelopment/VS_Code/GMACS_tpl-cpp_code/_build";
p2d = "~/Work/StockAssessments-Crab/Assessments/TannerCrab/2024-05_TannerCrab/Models-GMACS/InputFiles/TannerCrab24_01"
if (FALSE){
  #--run jitter evaluation----
  runJitter(path2out="testing/jittering_runs",
            path2exe=p2e,
            path2dat=p2d,
            numRuns=3,
            minPhase=4,
            onlyEvalJitter=FALSE,
            out.csv='jitterResults.csv',
            cleanup=TRUE,
            keepFiles=c("tmp.sh","gmacs.par"),
            cleanupAll=FALSE,
            test=FALSE)
}

if (FALSE){
  #--run evaluation-only----
  runJitter(path2out="testing/jittering_runs",
            path2exe=p2e,
            path2dat=p2d,
            numRuns=3,
            minPhase=4,
            onlyEvalJitter=TRUE,
            out.csv='jitterResults.csv',
            cleanup=TRUE,
            keepFiles=c("tmp.sh","gmacs.par"),
            cleanupAll=FALSE,
            test=FALSE)
}

