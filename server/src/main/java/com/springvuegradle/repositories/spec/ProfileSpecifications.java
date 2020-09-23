package com.springvuegradle.repositories.spec;

import com.springvuegradle.model.ActivityType;
import com.springvuegradle.model.Email;
import com.springvuegradle.model.Profile;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.Join;

import java.util.Objects;


/**
 * Helper class containing methods to create Specifications to use in ProfileRepository. Specifications can be passed to
 * findAll() to filter results. The specifications can be chained using the and()/or()/not() etc. methods to create more
 * complex criteria (e.g. (nickname matches * pattern X and firstname matches pattern Y) or lastname matches pattern Z);
 * see the Specification javadoc page.
 */
public class ProfileSpecifications {

    /**
     * Private constructor for class.
     */
    private ProfileSpecifications() {
        throw new UnsupportedOperationException("Cannot instantiate helper class.");
    }

    /**
     * Creates a specification matching all profiles that are not the default admin (i.e. have an authorization level of
     * 1 or more)
     * @return A specification matching all profiles that are not the default admin.
     */
    public static Specification<Profile> notDefaultAdmin() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.greaterThan(root.get("authLevel"), 0);
    }

    /**
     * Creates a specification matching all profiles whose first name contains the given substring.
     * @param substring The string pattern to be matched to.
     * @return A specification matching all profiles whose first name contains the given substring.
     */
    public static Specification<Profile> firstNameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                criteriaBuilder.lower(root.get("firstname")), pattern);
    }

    /**
     * Creates a specification matching all profiles whose first name equals the given string.
     * @param string The string to be matched to.
     * @return A specification matching all profiles whose first name equals the given string.
     */
    public static Specification<Profile> firstNameEquals(String string) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                root.get("firstname"), string);
    }

    /**
     * Creates a specification matching all profiles whose middle name contains the given substring.
     * @param substring The string pattern to be matched to.
     * @return A specification matching all profiles whose middle name contains the given substring.
     */
    public static Specification<Profile> middleNameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                criteriaBuilder.lower(root.get("middlename")), pattern);
    }

    /**
     * Creates a specification matching all profiles whose middle name equals the given string.
     * @param string The string to be matched to.
     * @return A specification matching all profiles whose middle name equals the given string.
     */
    public static Specification<Profile> middleNameEquals(String string) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                root.get("middlename"), string);
    }

    /**
     * Creates a specification matching all profiles whose nickname contains the given substring.
     * @param substring The string pattern to be matched to.
     * @return A specification matching all profiles whose nickname contains the given substring.
     */
    public static Specification<Profile> nicknameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                criteriaBuilder.lower(root.get("nickname")), pattern);
    }

    /**
     * Creates a specification matching all profiles whose nickname equals the given string.
     * @param string The string to be matched to.
     * @return A specification matching all profiles whose nickname equals the given string.
     */
    public static Specification<Profile> nicknameEquals(String string) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                root.get("nickname"), string);
    }

    /**
     * Creates a specification matching all profiles whose last name contains the given substring.
     * @param substring The string pattern to be matched to.
     * @return A specification matching all profiles whose last name contains the given substring.
     */
    public static Specification<Profile> lastNameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";

        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                criteriaBuilder.lower(root.get("lastname")), pattern);
    }

    /**
     * Creates a specification matching all profiles whose last name equals the given string.
     * @param string The string to be matched to.
     * @return A specification matching all profiles whose last name equals the given string.
     */
    public static Specification<Profile> lastNameEquals(String string) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(
                root.get("lastname"), string);
    }

    /**
     * Creates a specification matching all profiles that has an associated email address containing the given substring.
     * @param address The email address to be matched to.
     * @return A specification matching all profiles that has an associated email address containing the given substring.
     */
    public static Specification<Profile> emailContains(String address) {
        String pattern = "%" + address.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> {
            query.distinct(true);
            Join<Profile, Email> profileEmailJoin = root.joinSet("emails");
            return criteriaBuilder.like(criteriaBuilder.lower(profileEmailJoin.get("address")), pattern);
        };
    }

    /**
     * Creates a specification to match profiles that have the provided activity types.
     * @param types The collection of activity types to me matched against.
     * @param searchMethod true if only profiles that match all activity types should be provided. If false, any profile
     *                     that has any of the types given will be returned.
     * @return a specification that matches profiles that have the provided activity types.
     */
    public static Specification<Profile> activityTypesContains(String[] types, String searchMethod) {
        Specification<Profile> spec = Specification.where(null);
        for (int i = 0; i < types.length; i++) {
            final String type = types[i];
            Specification<Profile> activitySpec = (root, query, criteriaBuilder) -> {
                query.distinct(true);
                Join<Profile, ActivityType> groupJoin = root.joinSet("activityTypes");
                return criteriaBuilder.equal(groupJoin.get("activityTypeName"), type);
            };

            if (searchMethod.equals("all")) {
                spec = spec.and(activitySpec);
            } else {
                spec = spec.or(activitySpec);
            }
        }
        return spec;
    }

    /**
     * Creates a specification matching all profiles whose first, middle, last, or nickname contains the given substring.
     * @param substring The string pattern to be matched to.
     * @return A specification matching all profiles whose middle first, middle, last, or nickname contains the given substring.
     */
    public static Specification<Profile> anyNameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        Specification<Profile> checkFirstName = (root, query, criteriaBuilder) ->
            criteriaBuilder.like(
                    criteriaBuilder.lower(root.get("firstname")), pattern);
        Specification<Profile>  checkMiddleName = (root, query, criteriaBuilder) ->
                criteriaBuilder.like(
                        criteriaBuilder.lower(root.get("middlename")), pattern);
        Specification<Profile>  checkLastName = (root, query, criteriaBuilder) ->
                criteriaBuilder.like(
                        criteriaBuilder.lower(root.get("lastname")), pattern);
        Specification<Profile>  checkNickname = (root, query, criteriaBuilder) ->
                criteriaBuilder.like(
                        criteriaBuilder.lower(root.get("nickname")), pattern);
        return Objects.requireNonNull(Objects.requireNonNull(checkFirstName.or(checkMiddleName)).or(checkLastName)).or(checkNickname);
    }
}
