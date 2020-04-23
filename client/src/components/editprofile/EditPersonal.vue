<template>

    <div class="container">

        <h1 class="title is-5">Edit Basic Info</h1>

        <form @submit.prevent="sendUpdatedData">
            <b-field group-multiline grouped>
                <b-field label="First Name" expanded >
                    <b-input v-model="firstName" placeholder="First Name" required></b-input>
                </b-field>
                <b-field label="Middle Name" expanded>
                    <b-input v-model="middleName" placeholder="Middle Name"></b-input>
                 </b-field>
                <b-field label="Last Name" expanded>
                    <b-input v-model="lastName" placeholder="Last Name" required></b-input>
                </b-field>
            </b-field>

            <b-field label="Nickname" expanded>
                <b-input v-model="nickName" type="text" placeholder="Nickname"></b-input>
            </b-field>


            <b-field group-multiline grouped>
            <b-field label="Date of Birth" expanded>

                <b-datepicker
                        editable
                        :use-html5-validation="false"
                        placeholder="Select Date of Birth"
                        :min-date="minDate"
                        :max-date="maxDate"
                        ref="dateOfBirth"
                        v-model="dateOfBirth"
                        type="date" required
                        validation-message="Please enter a valid date"
                        pattern="^(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\/|-|\.)(?:0?[13-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$">>
                </b-datepicker>
            </b-field>

            <b-field label="Gender" expanded>
                <b-select
                        placeholder="Choose a gender"
                        v-model="gender" required expanded>
                    <option value="female">Female</option>
                    <option value="male">Male</option>
                    <option value="non-Binary">Non-Binary</option>
                </b-select>
            </b-field>
            </b-field>

            <b-field label="Fitness Level" expanded >
                <b-select v-model="fitness_level" placeholder="Fitness Level" expanded>
                    <option value="0">Beginner: I am not active at all </option>
                    <option value="1">Novice: I do a low level of exercise (walking)</option>
                    <option value="2">Intermediate: I work out 1-2 times per week</option>
                    <option value="3">Advanced: I work out 3-4 times per week</option>
                    <option value="4">Pro: I work out 5+ times per week</option>
                </b-select>
            </b-field>
            <b-field label="Bio" expanded>
                <b-input v-model="bio" maxlength="200" type="textarea" placeholder="Enter a bio"></b-input>
            </b-field>
            <b-field>
                <b-button type="is-info" native-type="submit">Save</b-button>
            </b-field>
        </form>

    </div>
</template>

<script>
    import api from "../../Api";
    import authenticationStore from "../../store/authenticationStore";
    import profileStore from "../../store/profileStore";

    export default {
        name: "EditPersonal",
        data() {
            const today = new Date()
            return {
                firstName: profileStore.data.firstName,
                lastName: profileStore.data.lastName,
                middleName: profileStore.data.middleName,
                nickName: profileStore.data.nickname,
                bio: profileStore.data.bio,
                dateOfBirth: new Date(profileStore.data.dateOfBirth),
                gender: profileStore.data.gender,
                fitness_level: profileStore.data.fitnessLevel,
                fitness_statement: null,
                date: profileStore.data.dateOfBirth,
                maxDate: new Date(today.getFullYear(), today.getMonth(), today.getDate()),
                minDate: new Date(today.getFullYear() -100, today.getMonth(), today.getDate())
            }
        },
        mounted() {
            switch(this.fitness_level) {
                case 0 :
                    this.fitness_statement = "Beginner: I am not active at all";
                    break;
                case 1 :
                    this.fitness_statement = "Novice: I do a low level of exercise (walking)";
                    break;
                case 2 :
                    this.fitness_statement = "Intermediate: I work out 1-2 times per week";
                    break;
                case 3 :
                    this.fitness_statement = "Advanced: I work out 3-4 times per week";
                    break;
                case 4 :
                    this.fitness_statement = "Pro: I work out 5+ times per week";
                    break;
                default:
                    this.fitness_statement = "Beginner: I am not active at all";
            }
        },

        methods: {
            sendUpdatedData(){
                const personalDetails = {"firstname": this.firstName,
                                 "lastname": this.lastName,
                                "middlename": this.middleName,
                                "nickname": this.nickName,
                                "bio": this.bio,
                                "date_of_birth": this.dateOfBirth,
                                "gender": this.gender,
                                "fitness": this.fitness_level
                }
                profileStore.methods.updatePersonal(personalDetails)
                const updatedProfile = {
                    "lastname": profileStore.data.lastName,
                    "firstname": profileStore.data.firstName,
                    "middlename": profileStore.data.middleName,
                    "nickname": profileStore.data.nickname,
                    "primary_email": profileStore.data.primaryEmail,
                    "additional_email": profileStore.data.optionalEmails,
                    "bio": profileStore.data.bio,
                    "date_of_birth": profileStore.data.dateOfBirth,
                    "gender": profileStore.data.gender,
                    "fitness": profileStore.data.fitnessLevel,
                    "passports":profileStore.data.passportCountries,
                    "activities": profileStore.data.activityTypes
                }
                api.editProfile(authenticationStore.methods.getUserId(), updatedProfile, authenticationStore.methods.getSessionId())
                    .catch(error => this.showError(this.displayError(error.response.status)))
                    .then(response => this.showMessage(this.displayError(response.status)))
            },
            showMessage(message) {
                this.$buefy.toast.open({
                    duration: 2000,
                    message: message,
                    type: 'is-success',
                    position: 'is-top'
                })
            },
            showError(message) {
                this.$buefy.toast.open({
                    duration: 2000,
                    message: message,
                    type: 'is-danger',
                    position: 'is-top'
                })
            },
            displayError(statusCode) {
                let message = ""
                if (statusCode == 200) {
                    message = "Details updated successfully"
                } else if (statusCode == 400 || statusCode == 403 || statusCode == 401) {
                    message = "Please fill in all required fields"
                }
                return message;
            }
        }
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
        margin-top: 0px;
        padding: 0px;
    }


</style>