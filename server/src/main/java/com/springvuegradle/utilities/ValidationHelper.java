package com.springvuegradle.utilities;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.springvuegradle.model.PassportCountry;
import com.springvuegradle.model.Profile;
import com.springvuegradle.repositories.PassportCountryRepository;
import com.springvuegradle.repositories.ProfileRepository;

import java.io.IOException;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ValidationHelper {

    private ValidationHelper(){
        throw new IllegalStateException("Utility class should not be instantiated");
    }

    /**
     * Connects to RESTcountries API and retrieves countries as a JSON file before converting them into a set of
     * PassportCountry objects.
     *
     * @returns a set of countries provided by the API converted into PassportCountry objects.
     * @throws IOException (URL Type needs IOException for whatever reason)
     * @author Matthew Wong
     * @author Alan Wang
     * @author Hamesh Ravji
     */
    public static Set<PassportCountry> GetRESTCountries() throws IOException {
        URL restCountries = new URL("https://restcountries.eu/rest/v2/all?fields=name;numericCode");
        HttpURLConnection connection = (HttpURLConnection) restCountries.openConnection();
        connection.setRequestMethod("GET");
        Set<PassportCountry> countries = new HashSet<>();
        ObjectMapper mapper = new ObjectMapper();

        try (java.io.InputStream in = new java.net.URL("https://restcountries.eu/rest/v2/all?fields=name;numericCode").openStream()) {
            String data = new String(in.readAllBytes());
            countries.addAll(Arrays.asList(mapper.readValue(data, PassportCountry[].class)));
        }
        return countries;
    }

    /**
     * Checks if a country is contained within a list of country strings
     * @param testCountry a PassportCountry Object being tested
     * @param countries the list of country strings to test membership of
     * @return boolean result of the test
     */
    public static boolean validateCountry(PassportCountry testCountry, List<String> countries) {
        boolean test = false;
        for (String country : countries) {
            if(country.equals(testCountry.getCountryName())) {
                test = true;
                break;
            }
        }
        return test;
    }

    /**
     * This method updates both the Profile repository as well as the Passport Country repository by checking against the
     * countries imported by the REST Countries API. If the country is no longer in the API, it is removed. New countries
     * are added to the Passport Country repository.
     *
     * Note that some countries that do not have an ISO 3166-1 numeric code (only the Republic of Kosovo at the time of
     * writing) cannot be included in
     * @param pcRepository the passport country repository to be saved to.
     * @param repository the profile repository to be saved to.
     * @throws IOException if there is an error retrieving from the RESTcountries API.
     */
    public static void updatePassportCountryRepository(PassportCountryRepository pcRepository, ProfileRepository repository) throws IOException {

        // adding new countries to the passport country repository if they do not already exist in the repository.
        Set<PassportCountry> updatedAPICountries = GetRESTCountries();
        int assignedCodeCounter = 900;
        for (PassportCountry country: updatedAPICountries) {
            //if country has no numeric code, assign a free code from 900-999 (free block in ISO 3166-1)
            if (country.getNumericCode() == null){
                if (pcRepository.existsByCountryName(country.getCountryName())) {
                    continue;
                } else {
                    while (assignedCodeCounter < 1000 && pcRepository.existsByNumericCode(Integer.toString(assignedCodeCounter))){
                        assignedCodeCounter++;
                    }
                    if (assignedCodeCounter < 1000){
                        country.setNumericCode(Integer.toString(assignedCodeCounter));
                    } else {
                        throw new IOException("Database error: No available user-assigned codes remaining");
                    }
                }
            }
            List<PassportCountry> result = pcRepository.findByNumericCode(country.getNumericCode());
            if (pcRepository.findByNumericCode(country.getNumericCode()).isEmpty()) {
                pcRepository.save(country);
            } else {
                PassportCountry entry = result.get(0);
                entry.setCountryName(country.getCountryName());
                pcRepository.save(entry);
            }
        }
        // removing all the passport countries not part of the API from each user if they are not in the passport country repository
        List<Profile> allProfiles = repository.findAll();
        for (Profile profile: allProfiles) {
            profile.getPassportObjects().removeIf(country -> !updatedAPICountries.contains(country));
            repository.save(profile);
        }
        // removing all the passport countries which are in the repository but not in the API
        for (PassportCountry passportCountry: pcRepository.findAll()) {
            if (!updatedAPICountries.contains(passportCountry) && !passportCountry.getCountryName().equals("Republic of Kosovo")) {
                pcRepository.delete(passportCountry);
            }
        }
    }
}