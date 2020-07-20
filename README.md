# Seng302 Team 100 - Wander
-----
### Requirements
The program can be run on a Windows or Linux operating system with Java/JDK 11. 
NPM and Gradle will also likely be required. 

### Building and Running the Program

1. Download a copy of the repository or clone with HTTPS by running `git clone https://eng-git.canterbury.ac.nz/seng302-2020/team-100.git`

#### Front End
2. Run `npm install` in the 'client' directory of the project
3. Run `npm run serve` in the same directory
4. The frontend of the program should now be running and accessible at http://localhost:9500/

#### Back End
5. In the 'server' directory of the project, run `gradlew bootJar` on Windows or `./gradlew bootJar` on Linux
6. The jar file should be generated inside the server/build/libs directory.  
6b. If on Linux, you may have to add execute permission to the jar with `chmod +x server-0.0.1-SNAPSHOT.jar`
7. Running `Java -jar server-0.0.1-SNAPSHOT.jar` in this directory should execute the jar. 

Note: Certain credentials may be required to be set as environment variables 

### Remote Server Access
 - The latest production version of the program can be found at https://csse-s302g1.canterbury.ac.nz/prod/
 - The latest development version of the program can be found at https://csse-s302g1.canterbury.ac.nz/test/
 - Sonarqube analysis for the program can be found at https://csse-s302g1.canterbury.ac.nz/sonarqube/

### Example Login Data

 - To log in with a regular profile, use the Email `Steve@test.com` with the password `987654321`.  
 - To log in with an admin profile, use the Email `Dave@test.com` with the password `SecureAdminPassword`.
 
For full instructions on how to use the application, please refer to our [User Manual](https://eng-git.canterbury.ac.nz/seng302-2020/team-100/wikis/User-Manual) 

### Licensing

Add the licensing for each of our dependencies (business cases) here

##### Buefy
(Includes node-sass and sass-loader)
Buefy is licensed under the MIT License
Copyright (c) 2017-2019 Rafael Beraldo

`Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:`

`The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.`

`THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.`  
  
##### Vue-router
Vue-router is licensed under the MIT License  
Copyright (c) 2013-present Evan You  
`[SEE ABOVE]`

##### VueX
Vuex is licensed under the MIT License  
Copyright (c) 2015-present Evan You  
`[SEE ABOVE]`
  
##### Vee-Valitade
Vee-validate is licensed under the MIT License 
`[SEE ABOVE]`

##### Jwt-Decode
Jwt-decode is licensed under the MIT License 
Copyright (c) 2015 Auth0, Inc
`[SEE ABOVE]`
 
##### JsonWebToken
JsonWebToken is licensed under the MIT License 
Copyright (c) 2015 Auth0, Inc
`[SEE ABOVE]`

### Reference
- [Spring Boot Docs](https://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/)
- [Spring JPA docs](https://docs.spring.io/spring-data/jpa/docs/current/reference/html/#preface)
- [Vue docs](https://vuejs.org/v2/guide/)