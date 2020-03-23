package com.springvuegradle.Controller;

import com.springvuegradle.Controller.enums.AuthenticationErrorMessage;
import com.springvuegradle.Model.*;
import com.springvuegradle.Repositories.EmailRepository;
import com.springvuegradle.Repositories.PassportCountryRepository;
import com.springvuegradle.dto.ChangePasswordRequest;
import com.springvuegradle.dto.EmailAddRequest;
import com.springvuegradle.dto.EmailUpdateRequest;
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
    private ProfileRepository repository;

    /**
     * Way to access PassportCountry Repository (Passport Country table in db).
     */
    @Autowired
    private PassportCountryRepository pcRepository;

    /**
     * Way to access Email Repository (Email table in db).
     */
    @Autowired
    private EmailRepository eRepository;

    /**
     * Way to access methods of the Login Controller, such as for checking authentication.
     */
    private LoginController loginController = new LoginController();

    /**
     * Endpoint for creating profiles.
     * Creates a new Profile object given a set of JSON data and forms a profile object based on the given data, then
     * hashes the password and adds the new data to the database.
     * @param newProfile contains data relating to the user profile we wish to add to the database.
     * @return the created profile and/or status code.
     */
    @PostMapping("/profiles")
    public ResponseEntity<String> createProfile (@RequestBody Profile newProfile) {
        String error = verifyProfile(newProfile, false);
        if (error.equals("")) {
            String hashedPassword = hashPassword(newProfile.getPassword());
            if (hashedPassword != "Hash Failed") {
                newProfile.setPassword(hashedPassword);
            } else {
                return new ResponseEntity<>("Error hashing password.", HttpStatus.INTERNAL_SERVER_ERROR);
            }
            Set<PassportCountry> updated = new HashSet<PassportCountry>();
            for(PassportCountry passportCountry : newProfile.getPassportObjects()){
                List<PassportCountry> result = pcRepository.findByCountryName(passportCountry.getCountryName());{
                    updated.add(result.get(0));
                }
            }
            newProfile.setPassports(updated);
            repository.save(newProfile);
            saveEmails(newProfile);
            return new ResponseEntity<>("New profile has been created.", HttpStatus.CREATED);
        } else {
            return new ResponseEntity<>(error, HttpStatus.FORBIDDEN);
        }
    }

    /**
     * Endpoint for getting profiles.
     * @param id referencing the profile
     * @param sessionToken to check if the user is logged in
     * @return response entities holding a profile object returned, sent to the front-end as json data in the format defined
     * by the Profile model class, along with a status code.
     */
    @GetMapping("/profiles/{id}")
    public @ResponseBody ResponseEntity<Profile> getProfile(@PathVariable Long id, @RequestHeader("authorization") Long sessionToken) {
        if (loginController.checkCredentials(id, sessionToken)) {
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
     * @param sessionToken the token containing this profile's session key, pulled from the request header.
     * @return An HTTP response with an appropriate status code and the updated profile if there method was successful.
     */
    @PutMapping("/profiles/{id}")
    public @ResponseBody ResponseEntity<String> updateProfile(@RequestBody Profile editedProfile, @RequestHeader("authorization") Long sessionToken, @PathVariable Long id) {
        if (loginController.checkCredentials(id, sessionToken)) {
            return updateProfile(editedProfile, id);
        } else {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(), HttpStatus.UNAUTHORIZED);
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
        Optional<Profile> result = repository.findById(id);
        if (Boolean.TRUE.equals(result.isPresent())) {
            Profile profileToDelete = result.get();
            for (Email email: profileToDelete.retrieveEmails()) {
                eRepository.delete(email);
            }
            repository.delete(profileToDelete);
            return new ResponseEntity<>("The Profile does exist in the database.", HttpStatus.OK);
        } else {
            return new ResponseEntity<>("The profile does not exist in the database.", HttpStatus.NOT_FOUND);
        }
    }

    /**
     * Endpoint for adding emails. Not used by front-end but is required by the specification.
     * @param request form containing a list of emails
     * @param id referring to the profile
     * @param sessionToken to check if the user is currently authenticated
     * @return response entity which can return a string if there is an error, all cases will return status code
     */
    @PostMapping("/profiles/{id}/emails")
    public @ResponseBody ResponseEntity<String> addEmails(@RequestBody EmailAddRequest request, @PathVariable Long id, @RequestHeader("authorization") Long sessionToken) {
        return addEmails(request, id, sessionToken, false);
    }

    /**
     * Called by the endpoint defined above
     * @param testing indicates whether called from test or endpoint. Tests can skip authentication.
     */
    protected ResponseEntity<String> addEmails(EmailAddRequest request, Long id, Long sessionToken, Boolean testing) {
        HttpStatus status;
        String message;
        Long sessionID = sessionToken;
        if (testing || loginController.checkCredentials(id, sessionID)) {
            Optional<Profile> result = repository.findById(id);
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
     * @param sessionToken session ID generated at login that is associated with this profile, pulled from the request header.
     * @return An HTTP response with an appropriate status code and, if there was a problem with the request, an error message.
     */
    @PutMapping("/profiles/{id}/emails")
    public ResponseEntity<String> editEmails (@RequestBody EmailUpdateRequest newEmails, @PathVariable Long id, @RequestHeader("authorization") Long sessionToken) {
        if (loginController.checkCredentials(id, sessionToken)) {
            return editEmails (newEmails, id);
        } else {
            return new ResponseEntity<>(AuthenticationErrorMessage.INVALID_CREDENTIALS.getMessage(), HttpStatus.UNAUTHORIZED);
        }
    }

    /**
     * Updates a user's password. Completes verification to ensure that the old password is correct and the two new passwords match.
     * @param newPasswordRequest form contains old password as well as two strings which are both the new password and should be identical
     * @param id the id of the profile we want to change the password for
     * @param sessionToken used to check if the user is authenticated
     * @return response entity which can return a string if there is an error, all cases will return status code
     */
    @PutMapping("/profiles/{id}/password")
    public ResponseEntity<String> changePassword (@RequestBody ChangePasswordRequest newPasswordRequest, @PathVariable Long id, @RequestHeader("authorization") Long sessionToken) {
        return changePassword(newPasswordRequest, id, sessionToken, false);
    }

    public ResponseEntity<String> changePassword(ChangePasswordRequest newPasswordRequest, Long id, Long sessionToken, Boolean testing) {
        if(!testing && !loginController.checkCredentials(id.intValue(), sessionToken)){
            return new ResponseEntity<>("Invalid session ID.", HttpStatus.UNAUTHORIZED);
        }

        if (newPasswordRequest.getNewPassword().length() < 8) {
            return new ResponseEntity<>("Password must be 8 characters or greater.", HttpStatus.FORBIDDEN);
        }

        Optional<Profile> result = repository.findById(id);
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
        repository.save(dbProfile);
        return new ResponseEntity<>("Successfully changed password.", HttpStatus.OK);
    }

    /**
     * Takes the plaintext password and hashes it
     * @param plainPassword the plaintext password to input
     * @return the hashed password
     */
    protected static String hashPassword(String plainPassword) {
        try {
            MessageDigest hashedPassword = MessageDigest.getInstance("SHA-256");
            return DatatypeConverter.printHexBinary(hashedPassword.digest(plainPassword.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException error) {
            System.out.println(error);
        }
        String failPassword = "Hash Failed";
        return failPassword;
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
        if (Boolean.FALSE.equals(eRepository.existsByAddress(newAddress))) {
            Email emailToAdd = new Email(newAddress);
            isAdded = profile.addEmail(emailToAdd);
            if (isAdded) {
                eRepository.save(emailToAdd);
                repository.save(profile);
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
        Optional<Profile> profileWithId = repository.findById(id);
        if (profileWithId.isPresent()) {
            return new ResponseEntity(profileWithId.get(), HttpStatus.OK);
        } else {
            return new ResponseEntity(null, HttpStatus.NOT_FOUND);
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
        String verificationMsg = verifyProfile(editedProfile, true);
        if (!verificationMsg.equals("")) {
            return new ResponseEntity<>(verificationMsg, HttpStatus.BAD_REQUEST);
        }

        // verifying passport countries
        Profile db_profile = repository.findById(id).get();
        db_profile.updateProfileExceptEmailsPassword(editedProfile);
        Set<PassportCountry> updatedCountries = new HashSet<>();
        for(PassportCountry passportCountry : editedProfile.getPassportObjects()){
            List<PassportCountry> result = pcRepository.findByCountryName(passportCountry.getCountryName());
            updatedCountries.add(result.get(0));
        }
        db_profile.setPassports(updatedCountries);

        // verifying emails, reuses the editEmails method
        EmailUpdateRequest mockRequest = new EmailUpdateRequest(new ArrayList<String>(editedProfile.getAdditional_email()), editedProfile.getPrimary_email(), id.intValue());
        ResponseEntity<String> response = editEmails(mockRequest, id);
        if (!response.getStatusCode().equals(HttpStatus.OK)) {
            return new ResponseEntity<>(response.getBody(), response.getStatusCode());
        }

        repository.save(db_profile);
        return new ResponseEntity<>(db_profile.toString(), HttpStatus.OK);
    }

    /**
     * Updates a profile's emails in the database given a request to do so. This version contains a flag to disable authentication
     * for the purposes of automated testing.
     * @param newEmails The profile's new primary/additional emails embedded in an EmailUpdateRequest
     * @param id the ID of the profile being edited
     * @return An HTTP response with an appropriate status code and, if there was a problem with the request, an error message.
     */
    protected ResponseEntity<String> editEmails (EmailUpdateRequest newEmails, Long id){

        Profile db_profile = repository.findById(id).get();
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
            List<Email> emailsReturnedFromSearch = eRepository.findAllByAddress(primaryEmail);
            if (emailsReturnedFromSearch.isEmpty()) {
                newEmailSet.add(new Email(primaryEmail, true, db_profile));
            } else if (emailsReturnedFromSearch.get(0).getProfile().getId() == id) {
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
            List<Email> emailsReturnedFromSearch = eRepository.findAllByAddress(optionalEmail);
            if (emailsReturnedFromSearch.isEmpty()) {
                newEmailSet.add(new Email(optionalEmail, false, db_profile));
            } else if (emailsReturnedFromSearch.get(0).getProfile().getId() == id) {
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
                if (emailFromOldSet.getAddress() == emailFromNewSet.getAddress()) {
                    found = true;
                }
            }
            if (!found) {
                eRepository.delete(emailFromOldSet);
            }
        }

        // saving all the new and old emails to the database now that they have associated profile objects
        for (Email email: newEmailSet) {
            eRepository.save(email);
        }

        // assigning the emails to the profile and saving the profile object to the database
        db_profile.setEmails(newEmailSet);
        repository.save(db_profile);

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
            eRepository.save(email);
        }
    }

    /**
     * Used in the create profile and update profile methods for verification. Returns a string to let the user know what
     * was entered incorrectly.
     * @param newProfile object we want to complete verification on
     * @param edit_mode indicates whether the method is being called from the update profile method to skip email authentication
     * @return a string containing all the errors in the form, if any, else it will return an empty string
     */
    private String verifyProfile(Profile newProfile, boolean edit_mode) {
        String error = "";
        if (newProfile.retrievePrimaryEmail().getAddress() == "" ||
                newProfile.retrievePrimaryEmail().getAddress() == null) {
            error += "The email field is blank.\n";
        }
        if (newProfile.getFirstname() == "" ||
                newProfile.getFirstname() == null) {
            error += "The First Name field is blank.\n";
        }
        if (newProfile.getLastname() == "" ||
                newProfile.getLastname() == null) {
            error += "The Last Name field is blank.\n";
        }
        if (newProfile.getPassword().length() < 8) {
            error += "The Password is not long enough.\n";
        }
        if (newProfile.getFitness() > 4 || newProfile.getFitness() < 0) {
            error += "The fitness level isn't valid.\n";
        }
        if (newProfile.getDateOfBirth() == "" ||
                newProfile.getDateOfBirth() == null) {
            error += "The Date of Birth field is blank.\n";
        }
        if (!newProfile.getPassportObjects().isEmpty()) {
            for(PassportCountry passportCountry : newProfile.getPassportObjects()){
                if (!pcRepository.existsByCountryName(passportCountry.getCountryName())) {
                    error += String.format("Country %s does not exist in the database.", passportCountry.getCountryName());
                }
            }
        }
        if (!edit_mode) {
            error += verifyEmailsInProfile(newProfile);
        }


        if (!((newProfile.getGender().equals("male")) ||
                (newProfile.getGender().equals("female")) ||
                (newProfile.getGender().equals("non-Binary")))) {
            error += "The Gender field must contain either 'male', 'female' or 'non-binary'.\n";
        }
        return error;
    }

    /**
     * Method used to verify emails and check if the emails are already associated with a profile in the database.
     * @param newProfile object we want to verify the emails for
     * @return string containing error messages, if any, else empty string
     */
    private String verifyEmailsInProfile(Profile newProfile) {
        String error = "";
        if (newProfile.retrieveEmails().size() >= 1) {
            boolean valid = true;
            ArrayList<String> emailStrings = new ArrayList<>();
            for (Email email: newProfile.retrieveEmails()) {
                if (eRepository.existsByAddress(email.getAddress())) {
                    valid = false;
                } else {
                    emailStrings.add(email.getAddress());
                }
            }

            if (emailStrings.size() != (new HashSet(emailStrings)).size()) {
                error += "There are duplicate email addresses used. User cannot enter same email multiple times in primary" +
                        " and/or additional.\n";
            } else if (!valid) {
                error += "An email address you have entered is already in use by another Profile.\n";
            }
        }
        return error;
    }

}
