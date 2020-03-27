//Stores global state of the users profile
//Done this way to avoid complicated passing of data as props into seperate components
const profileStore = {
    data: {
        firstName: "",
        lastName: "",
        middleName: "",
        nickname: "",
        primaryEmail: "",
        optionalEmails:[],
        bio:"",
        dateOfBirth: "",
        gender: "",
        passportCountries: [],
        fitnessLevel: 1,
        activities: [],
        allActivities: []

    },
    methods: {
        setPassportCountries(passport_countries) {
            profileStore.data.passportCountries = passport_countries;
        },
        setPrimaryEmail(primary_email) {
            profileStore.data.primaryEmail = primary_email;
        },
        setActivities(activities) {
            profileStore.data.activities = activities;
        },
        setOptionalEmails(additional_email) {
            profileStore.data.optionalEmails = additional_email;
        },
        updatePersonal(updatedProfile) {
            profileStore.data.firstName = updatedProfile.firstname;
            profileStore.data.lastName = updatedProfile.lastname;
            profileStore.data.middleName = updatedProfile.middlename;
            profileStore.data.nickname = updatedProfile.nickname;
            profileStore.data.bio = updatedProfile.bio;
            profileStore.data.dateOfBirth = updatedProfile.date_of_birth;
            profileStore.data.gender = updatedProfile.gender;
            profileStore.data.fitnessLevel = updatedProfile.fitness;
        },
        setProfile(profile){
            profileStore.methods.updatePersonal(profile)
            profileStore.data.primaryEmail = profile.primary_email;
            profileStore.data.optionalEmails = profile.additional_email;
            profileStore.data.passportCountries = profile.passports;
            profileStore.data.activities = profile.activities;
        },
        setAllActivities(allActivities) {
            profileStore.data.allActivities = allActivities.allActivities;
        }
    }
};

export default profileStore