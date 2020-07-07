const sonarqubeScanner =  require('sonarqube-scanner');
sonarqubeScanner(
    {
        // replace "X" hereunder to match your VM url
        serverUrl:  'https://csse-s302g1.canterbury.ac.nz/sonarqube/',
        token: "f3af1b1a7c9cad62afe1f020d0885937a8849780",
        options : {
            'sonar.projectKey': 'team-100-client',
            'sonar.projectName': 'Team 100 - Client',
            "sonar.sourceEncoding": "UTF-8",
            'sonar.sources': 'src',
            'sonar.tests': 'src',
            'sonar.inclusions': '**',
            'sonar.test.inclusions': 'src/**/*.spec.js,src/**/*.test.js,src/**/*.test.ts',
            'sonar.typescript.lcov.reportPaths': 'coverage/lcov.info',
            'sonar.javascript.lcov.reportPaths': 'coverage/lcov.info',
            'sonar.testExecutionReportPaths': 'coverage/test-reporter.xml'
        }
    }, () => {});