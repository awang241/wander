package com.springvuegradle.repositories.spec;

import com.springvuegradle.model.Activity;
import com.springvuegradle.model.ActivityMembership;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.Join;

public class ActivitySpecifications {
    /**
     * Private constructor for class.
     */
    private ActivitySpecifications() {
        throw new UnsupportedOperationException("Cannot instantiate helper class.");
    }

    /**
     * Creates a specification matching all activities whose name contains the given substring. Matching is not case
     * sensitive.
     * @param substring the string pattern being matched against.
     * @return a specification matching all activities whose name contains the given substring.userIsCreator, userIsMemberOfRestricted,
     */
    public static Specification<Activity> nameContains(String substring) {
        String pattern = "%" + substring.toLowerCase() + "%";
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("activityName")), pattern);
    }

    /**
     * Creates a specification matching all activities with the given privacy level.
     * @param level the specified privacy level.
     * @return a specification matching all activities with the given privacy level.
     */
    public static Specification<Activity> hasPrivacyLevel(int level) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("privacyLevel"), level);
    }

    /**
     * Creates a specification matching all activities that have the given user as a member.
     * @param memberId the id of the user.
     * @return a specification matching all activities that have the given user as a member.
     */
    public static Specification<Activity> hasMember(long memberId) {
        return (root, query, criteriaBuilder) -> {
            Join<Activity, ActivityMembership> join = root.joinSet("members");
            return criteriaBuilder.equal(join.get("profile").get("id"), memberId);
        };
    }

    /**
     * Creates a specification matching all activities that have the given user as a member with the given role.
     * @param memberId the id of the user.
     * @param role the activity role the user must have.
     * @return a specification matching all activities that have the given user as a member with the given role.
     */
    public static Specification<Activity> hasMember(long memberId, ActivityMembership.Role role) {
        Specification<Activity> hasRole = (root, query, criteriaBuilder) -> {
            Join<Activity, ActivityMembership> join = root.joinSet("members");
            return criteriaBuilder.equal(join.get("role"), role);
        };
        return hasRole.and(hasMember(memberId));
    }
}
