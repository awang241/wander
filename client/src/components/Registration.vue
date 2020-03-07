<template>
    <div class="card">
        <div class="form-container sign-up-container">
            <h1>Create Account</h1>
            <form @submit.prevent="createUser" id="formRegister">
                <div class="required">
                    <input v-model="firstName" type="text" placeholder="First Name" name="First Name" id="firstName" maxlength="25" required>
                </div>
                <div>
                    <input v-model="middleName" type="text" placeholder="Middle Name" name="Last Name" id="middleName" maxlength="25">
                </div>
                <div class="required">
                    <input v-model="lastName" type="text" placeholder="Last Name" name="Last Name" id="lastName" maxlength="25" required>
                </div>
                <div class="required">
                    <input v-model="email" type="email" placeholder="Email" name="email" id="email" maxlength="40" required>
                </div>
                <div class="required">
                    <label for="dateOfBirth">Date of birth </label>
                    <input ref="dateOfBirth" v-model="dateOfBirth" type="date" name="dateOfBirth" id="dateOfBirth" required>
                </div>
                <div class="required">
                    <label for="gender">Gender: </label>
                    <select id="gender" name="gender" v-model="gender" required>
                        <option value="female">Female</option>
                        <option value="male">Male</option>
                        <option value="nonBinary">Non Binary</option>
                    </select>
                </div>

                <div class="required">
                    <input v-model="password" placeholder="Password" type="password" name="password" id="password" required>
                </div>
                <div>
                    <input type="text" name="nickname" placeholder="Nickname" id="nickname" maxlength="25" v-model="nickName">
                    <textarea v-model="bio" placeholder="Bio" id="bio" name="bio" rows="2" cols="30"></textarea>
                </div>
                <button class="btn btn-light" type="submit">Submit</button>
                <p>* Indicates a required field</p>
            </form>

        </div>
    </div>
</template>

<script>
    import axios from 'axios'
    import router from '../router.js'

    export default {
        name: "Registration",
        data() {
            return {
                firstName: "",
                lastName: "",
                middleName: "",
                nickName: "",
                email: "",
                password: "",
                bio: "",
                dateOfBirth: "",
                gender: "",
                allUsers: null
            }
        },
        mounted() {
            let today = new Date().toISOString().split('T')[0];
            this.$refs.dateOfBirth.setAttribute('max', today);

            axios.get("https://f91246de-53d1-425e-9b1b-5524c2b62a0e.mock.pstmn.io/getusers")
                .then(response => this.allUsers = response.data)
                .catch(error => console.log(error));

        },
        methods: {
            createUser() {
                let emailUsed = false;
                for (const user of this.allUsers.users) {
                    if (this.email == user.email) {
                        emailUsed = true;
                    }
                }
                if (emailUsed == false) {
                    axios.post('https://f91246de-53d1-425e-9b1b-5524c2b62a0e.mock.pstmn.io/createprofile', {
                        lastName: this.lastName,
                        firstName: this.firstName,
                        middleName: this.middleName,
                        nickName: this.nickName,
                        email: this.email,
                        password: this.password,
                        bio: this.bio,
                        date_of_birth: this.dateOfBirth,
                        gender: this.gender
                    })
                    router.push('Login');
                } else {
                    window.alert("The email you have entered is already registered. \n" +
                        "Please enter a different email.");
                }
            }
        }
    }
</script>


<style scoped>
    .card  {
        min-height: 700px;
    }

    h1 {
        padding: 1.5rem 0;
        font-weight: bold;
        margin: 0;
        text-align: center;
    }

    h2 {
        text-align: center;
    }

    form div{
        width: 100%;
    }

    form {
        background-color: #FFFFFF;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding: 0 50px;
        text-align: left;
    }



</style>
