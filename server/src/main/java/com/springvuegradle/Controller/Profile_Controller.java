package com.springvuegradle.Controller;

import com.springvuegradle.Controller.enums.AuthenticationErrorMessage;
import com.springvuegradle.Model.*;
import com.springvuegradle.Repositories.*;
import com.springvuegradle.Utilities.FieldValidationHelper;
import com.springvuegradle.Utilities.JwtUtil;
import com.springvuegradle.dto.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import com.springvuegradle.Repositories.ProfileRepository;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.xml.bind.DatatypeConverter;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.regex.Pattern;


/**
 * Profile Controller Class for handling Profile Models.
 * Contains most of the backend profiles endpoints.
 */
@RestController
public class Profile_Controller {

    /**
     * Way to access Profile repository (Profile table in db).
     */
    @Autowired
    private ProfileRepository repo;

    @Autowired
    private JwtUtil jwtUtil;

    /**
     * Way to access PassportCountry Repository (Passport Country table in db).
     */
    @Autowired
    private PassportCountryRepository pcRepo;

    /**
     * Way to access Email Repository (Email table in db).
     */
    @Autowired
    private EmailRepository eRepo;

    /**
     * Way to access ActivityType Repository (ActivityType table in db).
     */
    @Autowired
    private ActivityTypeRepository aRepo;

    /**
     * Way to access Activity Repository (Activity table in db).
     */
    @Autowired
    private ActivityRepository activityRepo;

    /**
     * Endpoint for creating profiles.
     * Creates a new Profile object given a set of JSON data and forms a profile object based on the given data, then
     * hashes the password and adds the new data to the database.
     * @param newProfile contains data relating to the user profile we wish to add to the database.
     * @return the created profile and/or status code.
     */
    @PostMapping("/profiles")
    public ResponseEntity<String> createProfile (@RequestBody Profile newProfile) {
        String error = FieldValidationHelper.verifyProfile(newProfile, false, pcRepo, aRepo, eRepo);
        if (error.equals("")) {
            String hashedPassword = hashPassword(newProfile.getPassword());
            if (!hashedPassword.equals("Hash Failed")) {
                newProfile.setPassword(hashedPassword);
            } else {
                return new ResponseEntity<>("Error hashing password.", HttpStatus.INTERNAL_SERVER_ERROR);
            }
            Set<PassportCountry> updated = new HashSet<>();
            for(PassportCountry passportCountry : newProfile.getPassportObjects()){
                List<PassportCountry> result = pcRepo.findByCountryName(passportCountry.getCountryName());
                updated.add(result.get(0));
            }
            newProfile.setPassports(updated);

            Set<ActivityType> updatedActivityType = new HashSet<>();
            for(ActivityType activityType : newProfile.getActivityTypeObjects()){
                List<ActivityType> resultActivityTypes = aRepo.findByActivityTypeName(activityType.getActivityTypeName());
                updatedActivityType.add(resultActivityTypes.get(0));
            }
            newProfile.setActivityTypes(updatedActivityType);



            repo.save(newProfile);
            saveEmails(newProfile);
            return new ResponseEntity<>("New profile has been created.", HttpStatus.CREATED);
        } else {
            return new ResponseEntity<>(error, HttpStatus.FORBIDDEN);
        }
    }

    /**
     * Endpoint for getting profiles.
     * @param id referencing the profile
     * @param token to check if the user is logged in
     * @return response entities holding a profile object returned, sent to the front-end as json data in the format defined
     * by the Profile model class, along with a status code.
     */
    @GetMapping("/profiles/{id}")
    public @ResponseBody ResponseEntity<Profile> getProfile(@PathVariable Long id, @RequestHeader("authorization") String token) {
        if (checkEditPermission(token, id)) {
            return getProfile(id);
        } else {
            return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
        }
    }

    /**
     * Endpoint for editing profiles.
     * Updates a profile in the database given a request to do so.
     * @param editedProfile a profile object created from the request
     * @param id the ID of the profile being edited, pulled from the URL as a path variable.
     * @param token the jwt token stored on the client
     * @return An HTTP response with an appropriate status code and the updated profile if there method was successful.
     */
    @PutMapping("/profiles/{id}")
    public @ResponseBody ResponseEntity<String> updateProfile(@RequestBody Profile editedProfile, @RequestHeader("authorization") String token, @PathVariable Long id) {
        if (checkEditPermission(token, id)) {
            return updateProfile(editedProfile, id);
        } else {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(), HttpStatus.UNAUTHORIZED);
        }
    }

    /**
     * Checks if user asking for permission to edit a profile has the right permission
     * @param token the jwt token stored on the client (user asking for permissions token)
     * @param id the id of the user being edited (can be null to check just the token permission)
     * @return true if user has permission to edit the profile, false otherwise
     */
    private boolean checkEditPermission(String token, Long id) {
        if (jwtUtil.validateToken(token) && (jwtUtil.extractPermission(token) == 0 || jwtUtil.extractPermission(token) == 1 || (jwtUtil.extractId(token).equals(id)))) {
            return true;
        } else {
            return false;
        }
    }


    /**
     * Deletes a profile from the repository given that it exists in the database. The method was initially used for
     * testing but might be useful for a later story.
     * @param id the id of the profile to be deleted
     * @return http response code and feedback message on the result of the delete operation
     */
    @DeleteMapping(value="/profiles/{id}")
    public @ResponseBody ResponseEntity<String> deleteProfile(@PathVariable Long id) {
        Optional<Profile> result = repo.findById(id);
        if (Boolean.TRUE.equals(result.isPresent())) {
            Profile profileToDelete = result.get();
            for (Email email: profileToDelete.retrieveEmails()) {
                eRepo.delete(email);
            }
            repo.delete(profileToDelete);
            return new ResponseEntity<>("The Profile does exist in the database.", HttpStatus.OK);
        } else {
            return new ResponseEntity<>("The profile does not exist in the database.", HttpStatus.NOT_FOUND);
        }
    }

    /**
     * Endpoint for adding emails. Not used by front-end but is required by the specification.
     * @param request form containing a list of emails
     * @param id referring to the profile
     * @param token the jwt token stored on the client
     * @return response entity which can return a string if there is an error, all cases will return status code
     */
    @PostMapping("/profiles/{id}/emails")
    public @ResponseBody ResponseEntity<String> addEmails(@RequestBody EmailAddRequest request, @PathVariable Long id, @RequestHeader("authorization") String token) {
        return addEmails(request, id, token, false);
    }

    /**
     * Endpoint for getting all profiles for admin
     * @param token to check if the user is currently authenticated
     * @return response entity which contains a list of simplified profiles and an ok status code or an unauthorized status code
     */
    @GetMapping("/profiles")
    public @ResponseBody ResponseEntity<List<SimplifiedProfileResponse>> getUserProfiles(@RequestHeader("authorization") String token) {
        if(jwtUtil.validateToken(token)) {
            return getUserProfiles(jwtUtil.extractPermission(token));
        } else {
            return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
        }
    }

    /**
     * Retrieves all profile data relevant for admin to view
     * @param authLevel the authentication level of the user
     * @return list of simplified profile responses
     */
    protected ResponseEntity<List<SimplifiedProfileResponse>> getUserProfiles(int authLevel) {
        List<Profile> profilesForAdmin = repo.findAllBelowAuthlevel(authLevel);
        List<SimplifiedProfileResponse> simplifiedProfiles = createSimplifiedProfiles(profilesForAdmin);
        return new ResponseEntity<>(simplifiedProfiles, HttpStatus.OK);
    }

    /**
     * Converts a list of normal profiles into a list of simplified profiles
     * @param profiles a list of normal profile objects to be simplified
     * @return a list of simplified profiles
     */
    protected List<SimplifiedProfileResponse> createSimplifiedProfiles(List<Profile> profiles) {
        List<SimplifiedProfileResponse> simplifiedProfiles = new ArrayList<>();
        for(Profile profile: profiles) {
            String tempEmail = eRepo.findPrimaryByProfile(profile);
            simplifiedProfiles.add(new SimplifiedProfileResponse(profile.getId(), profile.getFirstname(), profile.getLastname(),
                    tempEmail, profile.getGender()));
        }
        return simplifiedProfiles;
    }


    /**
     * Called by the endpoint defined above
     * @param testing indicates whether called from test or endpoint. Tests can skip authentication.
     */
    protected ResponseEntity<String> addEmails(EmailAddRequest request, Long id, String token, Boolean testing) {
        HttpStatus status;
        String message;
        if (testing || jwtUtil.validateToken(token)) {
            Optional<Profile> result = repo.findById(id);
            if (Boolean.TRUE.equals(result.isPresent())) {
                Profile targetProfile = result.get();
                for (String address: request.getEmails()) {
                    addAdditionalEmailToProfile(targetProfile, address);
                }
                status = HttpStatus.CREATED;
                message = "Emails added successfully.";
            } else {
                status = HttpStatus.FORBIDDEN;
                message = "That profile does not exist.";

            }
        } else {
            status = HttpStatus.UNAUTHORIZED;
            message = AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage();
        }

        return new ResponseEntity<>(message, status);
    }

    /**
     * Updates a profile's emails in the database given a request to do so.
     * @param newEmails The profile's new primary/additional emails embedded in an EmailUpdateRequest.
     * @param id the ID of the profile being edited, pulled from the URL as a path variable.
     * @param token the jwt token stored on the client
     * @return An HTTP response with an appropriate status code and, if there was a problem with the request, an error message.
     */
    @PutMapping("/profiles/{id}/emails")
    public ResponseEntity<String> editEmails (@RequestBody EmailUpdateRequest newEmails, @PathVariable Long id, @RequestHeader("authorization") String token) {
        if (jwtUtil.validateToken(token)) {
            return editEmails (newEmails, id);
        } else {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(), HttpStatus.UNAUTHORIZED);
        }
    }

    /**
     * Updates a profile's activityType types in the database given a request to do so.
     * @param newActivityTypes The profile's new primary/additional emails embedded in an EmailUpdateRequest.
     * @param id the ID of the profile being edited, pulled from the URL as a path variable.
     * @param token the jwt token stored on the client
     * @return An HTTP response with an appropriate status code and, if there was a problem with the request, an error message.
     */
    @PutMapping("/profiles/{id}/activityType-types")
    public ResponseEntity<String> editActivityTypes (@RequestBody ActivityTypeUpdateRequest newActivityTypes, @PathVariable Long id, @RequestHeader("authorization") String token) {
        if (!jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(), HttpStatus.UNAUTHORIZED);
        }
        return editActivityTypes(newActivityTypes.getActivityTypes(), id);
    }

    /**
     * Queries the Database to find all the activityTypes.
     * @return a response with all the activityTypes in the database.
     */
    @GetMapping("/activityTypes")
    public ResponseEntity<ActivityTypesResponse> getActivityTypesList() {
        List<String> allActivityTypes = aRepo.findAllActivityTypeNames();
        ActivityTypesResponse activityTypesResponse = new ActivityTypesResponse(allActivityTypes);
        return new ResponseEntity<>(activityTypesResponse, HttpStatus.OK);
    }



    /**
     * Updates a user's password. Completes verification to ensure that the old password is correct and the two new passwords match.
     * @param newPasswordRequest form contains old password as well as two strings which are both the new password and should be identical
     * @param id the id of the profile we want to change the password for
     * @param token the jwt token stored on the client
     * @return response entity which can return a string if there is an error, all cases will return status code
     */
    @PutMapping("/profiles/{id}/password")
    public ResponseEntity<String> changePassword (@RequestBody ChangePasswordRequest newPasswordRequest, @PathVariable Long id, @RequestHeader("authorization") String token) {
        return changePassword(newPasswordRequest, id, token, false);
    }

    public ResponseEntity<String> changePassword(ChangePasswordRequest newPasswordRequest, Long id, String token, Boolean testing) {
        if(!testing && !jwtUtil.validateToken(token)){
            return new ResponseEntity<>("Invalid session ID.", HttpStatus.UNAUTHORIZED);
        }

        if (newPasswordRequest.getNewPassword().length() < 8) {
            return new ResponseEntity<>("Password must be 8 characters or greater.", HttpStatus.FORBIDDEN);
        }

        Optional<Profile> result = repo.findById(id);
        if (result.isEmpty()) {
            return new ResponseEntity<>("Could not find profile in repository.", HttpStatus.NOT_FOUND);
        }
        Profile dbProfile = result.get();


        String dbHashedPassword = dbProfile.getPassword();

        if (!hashPassword(newPasswordRequest.getCurrentPassword()).equals(dbHashedPassword)) {
            return new ResponseEntity<>("Entered incorrect password.", HttpStatus.BAD_REQUEST);
        }

        if (!newPasswordRequest.getNewPassword().equals(newPasswordRequest.getConfPassword())) {
            return new ResponseEntity<>("New passwords do not match.", HttpStatus.BAD_REQUEST);
        }
        String newHashedPassword = hashPassword(newPasswordRequest.getNewPassword());
        dbProfile.setPassword(newHashedPassword);
        repo.save(dbProfile);
        return new ResponseEntity<>("Successfully changed password.", HttpStatus.OK);
    }

    /**
     * Takes the plaintext password and hashes it
     * @param plainPassword the plaintext password to input
     * @return the hashed password
     */
    public static String hashPassword(String plainPassword) {
        try {
            MessageDigest hashedPassword = MessageDigest.getInstance("SHA-256");
            return DatatypeConverter.printHexBinary(hashedPassword.digest(plainPassword.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException error) {
            System.out.println(error);
            return "Hash Failed";
        }
    }



    /**
     * This method adds a new email to a given profile, isPrimary set to false by default. This method will be called directly for testing,
     * when running, it will call the method with only profile and newAddress as parameters which sets testing to false and both repositories
     * to null so objects are saved to their actual repositories rather than given test repositories.
     * @param profile where we want to associate the email
     * @param newAddress new address we want to add
     * @return true if added, false otherwise
     */
    protected boolean addAdditionalEmailToProfile(Profile profile, String newAddress) {
        boolean isAdded = false;
        if (Boolean.FALSE.equals(eRepo.existsByAddress(newAddress))) {
            Email emailToAdd = new Email(newAddress);
            isAdded = profile.addEmail(emailToAdd);
            if (isAdded) {
                eRepo.save(emailToAdd);
                repo.save(profile);
            }
        }
        return isAdded;
    }

    /**
     * Retrieves data corresponding to the given profile ID from the database.
     * @param id gets the profile object and if it exists and authorization is approved, it will return the object
     * @return the Profile object corresponding to the given ID.
     */
    protected ResponseEntity<Profile> getProfile(Long id) {
        Optional<Profile> profileWithId = repo.findById(id);
        if (profileWithId.isPresent()) {
            return new ResponseEntity<>(profileWithId.get(), HttpStatus.OK);
        } else {
            return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);
        }
    }

    /**
     * Retrieves authentication level corresponding to the given token ID.
     * @param token gets the profile object and if it exists and authorization is approved, it will return the object
     * @return the Profile object corresponding to the given ID.
     */
    @GetMapping("/authLevel")
    protected ResponseEntity<Integer> getAuthLevel(@RequestHeader("authorization") String token) {
        if(jwtUtil.validateToken(token)) {
            return new ResponseEntity<>(jwtUtil.extractPermission(token), HttpStatus.OK);
        } else {
            return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
        }
    }


    /**
     * Updates a profile in the database given a request to do so.
     * @param editedProfile a profile object created from the request
     * @param id the ID of the profile being edited, pulled from the URL as a path variable.
     * @return An HTTP response with an appropriate status code and the updated profile if there method was successful.
     */
    protected ResponseEntity<String> updateProfile(Profile editedProfile, Long id){
        editedProfile.setPassword("temporary");
        String verificationMsg = FieldValidationHelper.verifyProfile(editedProfile, true, pcRepo, aRepo, eRepo);
        if (!verificationMsg.equals("")) {
            return new ResponseEntity<>(verificationMsg, HttpStatus.BAD_REQUEST);
        }

        // verifying passport countries
        Profile db_profile = repo.findById(id).get();
        db_profile.updateProfileExceptEmailsPassword(editedProfile);
        Set<PassportCountry> updatedCountries = new HashSet<>();
        for(PassportCountry passportCountry : editedProfile.getPassportObjects()){
            List<PassportCountry> result = pcRepo.findByCountryName(passportCountry.getCountryName());
            updatedCountries.add(result.get(0));
        }
        db_profile.setPassports(updatedCountries);

        // verifying activityTypes
        Set<ActivityType> updatedActivityTypes = new HashSet<>();
        for(ActivityType activityType : editedProfile.getActivityTypeObjects()){
            List<ActivityType> resultActivityTypes = aRepo.findByActivityTypeName(activityType.getActivityTypeName());
            updatedActivityTypes.add(resultActivityTypes.get(0));
        }
        db_profile.setActivityTypes(updatedActivityTypes);

        // verifying emails, reuses the editEmails method
        EmailUpdateRequest mockRequest = new EmailUpdateRequest(new ArrayList<String>(editedProfile.getAdditional_email()), editedProfile.getPrimary_email(), id.intValue());
        ResponseEntity<String> response = editEmails(mockRequest, id);
        if (!response.getStatusCode().equals(HttpStatus.OK)) {
            return new ResponseEntity<>(response.getBody(), response.getStatusCode());
        }

        repo.save(db_profile);
        return new ResponseEntity<>(db_profile.toString(), HttpStatus.OK);
    }

    /**
     * Updates the activityType types for a user
     * @param newActivityTypeStrings an arraylist of strings representing the new activityTypes
     * @param id the Id of the user we are editing
     * @return HTTP status code indicating if the operation was successful
     */
    protected ResponseEntity<String> editActivityTypes(ArrayList<String> newActivityTypeStrings, Long id){
        Profile profile = repo.findById(id).get();

        HashSet<ActivityType> newActivityTypes = new HashSet<>();
        for (String activityTypeString: newActivityTypeStrings) {

            //Check if the activityType is of a valid type
            if(!aRepo.existsByActivityTypeName(activityTypeString)){
                return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
            }
            newActivityTypes.add(aRepo.findByActivityTypeName(activityTypeString).get(0));
        }

        profile.setActivityTypes(newActivityTypes);
        repo.save(profile);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    /**
     * Updates a profile's emails in the database given a request to do so. This version contains a flag to disable authentication
     * for the purposes of automated testing.
     * @param newEmails The profile's new primary/additional emails embedded in an EmailUpdateRequest
     * @param id the ID of the profile being edited
     * @return An HTTP response with an appropriate status code and, if there was a problem with the request, an error message.
     */
    protected ResponseEntity<String> editEmails (EmailUpdateRequest newEmails, Long id){

        Profile db_profile = repo.findById(id).get();
        String primaryEmail = newEmails.getPrimary_email();

        Set<Email> newEmailSet = new HashSet<>();

        Set<Email> oldEmails = db_profile.retrieveEmails();

        List<String> duplicateDetectionList = new ArrayList<>();

        for (String emailString: newEmails.getAdditional_email()) {
            duplicateDetectionList.add(emailString);
        }
        if (newEmails.getPrimary_email() != null) {
            duplicateDetectionList.add(newEmails.getPrimary_email());
        }
        if (duplicateDetectionList.size() != new HashSet(duplicateDetectionList).size()) {
            return new ResponseEntity<>("Duplicate email addresses detected.", HttpStatus.FORBIDDEN);
        }

        if (duplicateDetectionList.size() > 5) {
            return new ResponseEntity<>("Cannot have more than 5 emails associated to a profile.", HttpStatus.FORBIDDEN);
        }

        for (String emailStr: duplicateDetectionList) {
            if (invalidEmail(emailStr)) {
                return new ResponseEntity<>("One of the emails is invalid as it does not have an @ symbol.", HttpStatus.BAD_REQUEST);
            }
        }

        // processing primary email to new emails set
        if(primaryEmail != null) {
            List<Email> emailsReturnedFromSearch = eRepo.findAllByAddress(primaryEmail);
            if (emailsReturnedFromSearch.isEmpty()) {
                newEmailSet.add(new Email(primaryEmail, true, db_profile));
            } else if (emailsReturnedFromSearch.get(0).getProfile().getId().equals(id)) {
                //case where primary email already associated with profile
                Email email = emailsReturnedFromSearch.get(0);
                email.setPrimary(true);
                newEmailSet.add(email);
            } else {
                return new ResponseEntity<>("Primary email address already in use by another profile.", HttpStatus.BAD_REQUEST);
            }
        } else {
            return new ResponseEntity<>("Primary address cannot be null.", HttpStatus.FORBIDDEN);
        }

        // processing additional emails to new emails set
        for (String optionalEmail: newEmails.getAdditional_email()) {
            List<Email> emailsReturnedFromSearch = eRepo.findAllByAddress(optionalEmail);
            if (emailsReturnedFromSearch.isEmpty()) {
                newEmailSet.add(new Email(optionalEmail, false, db_profile));
            } else if (emailsReturnedFromSearch.get(0).getProfile().getId().equals(id)) {
                Email email = emailsReturnedFromSearch.get(0);
                email.setPrimary(false);
                newEmailSet.add(email);
            } else {
                return new ResponseEntity<>("Email address already in use by another profile.", HttpStatus.FORBIDDEN);
            }
        }

        // removing email objects that are no longer in use from the database
        for (Email emailFromOldSet: oldEmails) {
            boolean found = false;
            for (Email emailFromNewSet: newEmailSet) {
                if (emailFromOldSet.getAddress().equals(emailFromNewSet.getAddress())) {
                    found = true;
                }
            }
            if (!found) {
                eRepo.delete(emailFromOldSet);
            }
        }

        // saving all the new and old emails to the database now that they have associated profile objects
        for (Email email: newEmailSet) {
            eRepo.save(email);
        }

        // assigning the emails to the profile and saving the profile object to the database
        db_profile.setEmails(newEmailSet);
        repo.save(db_profile);

        return new ResponseEntity<>(HttpStatus.OK);

    }

    /**
     * Validation pattern for emails strings to test they contain an @ with something either side
     * @param emailStr the string being tested
     * @return boolean of whether the string meets the validation
     */
    private boolean invalidEmail(String emailStr) {
        String pattern = ".*@.*";

        return !Pattern.matches(pattern, emailStr);
    }

    /**
     * Used in the create profile stage to save the new emails to the database, assumes verified already.
     * @param newProfile we want each email to be associated, also contains the email objects.
     */
    private void saveEmails(Profile newProfile) {
        Set<Email> emailsFromNewProfile = newProfile.retrieveEmails();
        for (Email email: emailsFromNewProfile) {
            email.setProfile(newProfile);
            eRepo.save(email);
        }
    }

    /**
     * Checks if a token is expired for the front end
     * @param token the token to be checked
     * @return An HTTP response with appropriate status code and a string to tell the front end that a token is expired or not
     */
    @GetMapping("/token")
    public @ResponseBody ResponseEntity<String> verifyToken(@RequestHeader("authorization") String token) {
        if (jwtUtil.validateToken(token)) {
            return new ResponseEntity<>("expired", HttpStatus.OK);
        } else {
            return new ResponseEntity<>("not expired", HttpStatus.UNAUTHORIZED);
        }
    }

}
