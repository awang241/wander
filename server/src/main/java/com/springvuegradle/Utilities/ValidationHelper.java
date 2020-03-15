package com.springvuegradle.Utilities;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.springvuegradle.Model.PassportCountry;
import com.springvuegradle.Model.Profile;
import com.springvuegradle.Repositories.PassportCountryRepository;
import com.springvuegradle.Repositories.ProfileRepository;

import java.io.IOException;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.*;

public class ValidationHelper {

    /**
     * Connects to RESTcountries API and retrieves countries as a JSON file before converting them into a set of
     * PassportCountry objects.
     *
     * @throws IOException (URL Type needs IOException for whatever reason)
     * @author Matthew Wong
     * @author Alan Wang
     */
    public static Set<PassportCountry> GetRESTCountries() throws IOException {
        URL restCountries = new URL("https://restcountries.eu/rest/v2/all?fields=name;numericCode");
        HttpURLConnection connection = (HttpURLConnection) restCountries.openConnection();
        connection.setRequestMethod("GET");
        Set<PassportCountry> countries = new HashSet<PassportCountry>();
        ObjectMapper mapper = new ObjectMapper();

        try (java.io.InputStream in = new java.net.URL("https://restcountries.eu/rest/v2/all?fields=name;numericCode").openStream()) {
            // Get the names of REST countries API
            String data = new String(in.readAllBytes());
            countries.addAll(Arrays.asList(mapper.readValue(data, PassportCountry[].class)));
            // Get each countries name alone and place them into an array "countries"
        } catch(ConnectException e) {
            e.printStackTrace();
            throw e;
        }
        return countries;
    }

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
     * @param pcRepository
     * @param repository
     * @throws IOException
     */
    public static void updatePassportCountryRepository(PassportCountryRepository pcRepository, ProfileRepository repository) throws IOException {

        // adding new countries to the passport country repository if they do not already exist in the repository.
        Set<PassportCountry> updatedAPICountries = GetRESTCountries();
        //updatedAPICountries.removeIf(country -> {return country.getNumericCode() == null;});
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
            if (pcRepository.findByNumericCode(country.getNumericCode()).size() == 0) {
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
            profile.retrievePassportCountryObjects().removeIf(country -> !updatedAPICountries.contains(country));
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