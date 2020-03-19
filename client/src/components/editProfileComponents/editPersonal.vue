<template>
<!--    <header> edit persasdfonadsfsadfal</header>-->
    <div class="container">

        <h1 class="Title">Edit Basic Info</h1>
<!--            <label class="label">Name</label>-->
<!--        <div class="field-body">-->
<!--            <div class="field">-->
<!--                <p class="control">-->
<!--                    -->
<!--                    -->
<!--                    -->
<!--                    -->
<!--                    -->
<!--                </p>-->
<!--            </div>-->
<!--        </div>-->
    <form @submit="sendUpdatedData">
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
                    :max-date="maxDate" ref="dateOfBirth"
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
            <b-input maxlength="200" type="textarea" placeholder="Enter a bio"></b-input>
        </b-field>

        <b-field>
            <b-button native-type="submit">Save</b-button>
        </b-field>
    </form>

    </div>
</template>

<script>
    import api from "../../Api";
    import authenticationStore from "../../store/authenticationStore";
    // import router from "../../router";

    export default {
        name: "editPersonal",
        data() {
            const today = new Date()
            return {
                firstName: "",
                lastName: "",
                middleName: "",
                nickName: "",
                bio: "",
                dateOfBirth: "",
                gender: null,
                fitness_level: null,
                fitness_statement: null,
                date: new Date(),
                maxDate: new Date(today.getFullYear(), today.getMonth(), today.getDate() + 5)
            }
        },
        mounted() {
            // Retrieves user data using their id number. Will change to token at some point
            api.getProfile(authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                .then((response) => {
                    console.log(response.data);
                    console.log(response.data.firstname)
                    this.firstName = response.data.firstname;
                    this.lastName = response.data.lastname;
                    this.middleName = response.data.middlename;
                    this.nickName = response.data.nickname;
                    this.dateOfBirth = response.data.date_of_birth;
                    this.gender = response.data.gender;
                    this.bio = response.data.bio;
                    this.email = response.data.email;
                    this.fitness_level = response.data.fitness_level;
                    switch(response.data.fitness_level) {
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
                })
                .catch(error => console.log(error));
        },

        methods: {
            sendUpdatedData(){
                const profile = {"firstName": this.firstName,
                                "middleName": this.middleName,
                                "lastName": this.lastName,
                                "dateOfBirth": this.dateOfBirth,
                                "gender": this.gender,
                                "fitnessLevel": this.fitnessLevel,
                                "bio": this.bio,
                                "nickname": this.nickname
                }
                api.editProfile(authenticationStore.methods.getUserId(), profile, authenticationStore.methods.getSessionId())
            }
        }
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
    }


</style>