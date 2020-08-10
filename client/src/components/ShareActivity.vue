<template>
    <div class="container">
        <h1 class="title">Share Activity</h1>
        <ValidationObserver v-slot="{ handleSubmit }">
            <form @submit.prevent="handleSubmit(shareActivity)">
                <ValidationProvider rules="required" name="Privacy" v-slot="{ errors, valid }" slim>
                    <b-field label="Activity Privacy"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Privacy<span class="requiredAsterix">*</span></template>
                        <b-select v-model="privacy" placeholder="Choose privacy setting" expanded>
                            <option value="private">Private</option>
                            <option value="friends">Restricted</option>
                            <option value="public">Public</option>
                        </b-select>
                    </b-field>
                </ValidationProvider>

                <div v-if="privacy == 'friends'">
                    <ValidationProvider rules="email" name="Email" v-slot="{ errors, valid }" slim>
                        <b-field label="Add friend's emails"
                                 :type="{'is-danger': errors[0], 'is-success': valid}"
                                 :message="errors"
                                 expanded>
                            <b-input type="email" v-model="newEmail" placeholder="Enter a friend's email" maxlength="30"
                                     expanded></b-input>
                        </b-field>
                    </ValidationProvider>
                    <b-button class="addButton" type="is-primary" @click="addEmail()">Add</b-button>
                    <br>
                    <br>

                    <div v-for="user in userRoles" v-bind:listItem="user.email" v-bind:key="user.email">
                        <ListItem v-bind:listItem="user.email" v-on:deleteListItem="deleteUser(user.email)">
                            <template>
                                <b-select v-model="user.role">
                                    <option value="follower">Follower</option>
                                    <option value="participant">Participant</option>
                                    <option value="organiser">Organiser</option>
                                </b-select>
                            </template>
                        </ListItem>
                    </div>
                </div>
                <br>
                <b-button style="float: right" @click="shareActivity"
                          type="is-primary">
                    Save
                </b-button>
                <b-button style="float: left" @click="goBack"
                          type="is-danger">
                    Cancel
                </b-button>
                <br>
            </form>
        </ValidationObserver>
    </div>
</template>


<script>
    import store from "../store";
    import router from "../router";
    import toastMixin from "../mixins/toastMixin";
    import {ValidationObserver, ValidationProvider} from "vee-validate";
    import Api from "../Api";
    import ListItem from "./ListItem";

    export default {
        name: "ShareActivity",
        mixins: [toastMixin],
        components: {
            ListItem,
            ValidationProvider,
            ValidationObserver
        },
        data() {
            return {
                privacy: null,
                userRoles: [],
                activityId: this.$route.params.id,
                newEmail: "",
                role: ""
            }
        },
        mounted() {
            this.checkAuthenticationStatus()
        },
        methods: {
            addEmail() {
                let emailAlreadyAdded = false
                this.userRoles.forEach(user => {
                    if (user.email === this.newEmail) {
                        emailAlreadyAdded = true
                    }
                })
                if(emailAlreadyAdded){
                    this.warningToast("Email has already been added!")
                    return;
                }
                if (this.newEmail === "" || this.newEmail.trim().length === 0 || !this.newEmail.includes('@', 0)) {
                    this.warningToast("Please enter a valid email address")
                    return;
                }
                Api.verifyEmail(this.newEmail)
                    .then((response) => {
                        if (response.data === true) {
                            this.userRoles.push({email: this.newEmail, role: "follower"});
                            this.newEmail = "";
                        } else {
                            this.warningToast("User with that email does not exist")
                        }
                    })
                    .catch(error => console.log(error));

            },
            shareActivity() {
                // need to check whether 'restricted' option is chosen. If yes, need to check list of users > 0
                Api.editActivityPrivacy(store.getters.getUserId, this.$route.params.id, this.privacy, localStorage.getItem('authToken'))
                    .then((response) => {
                        console.log(response);
                        this.successToast("Activity privacy updated")
                        router.go(-1)
                    })
                    .catch(error => console.log(error));
            },

            deleteUser(emailToDelete) {
                this.userRoles = this.userRoles.filter(user => user.email != emailToDelete)
            },

            goBack() {
                router.go(-1)
            },

            checkAuthenticationStatus() {
                if (!store.getters.getAuthenticationStatus) {
                    router.push({path: '/'})
                }
            },
        }
    }
</script>


<style scoped>
    .container {
        width: 800px;
    }

    @media only screen and (max-width: 600px) {
        .container {
            width: 100%;
        }
    }

    .requiredAsterix {
        color: red;
    }

    #roleSelection {

    }


</style>


