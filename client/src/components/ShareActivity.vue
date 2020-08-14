<template>
    <div class="container">
        <h1 class="title">Share Activity</h1>
        <ValidationObserver v-slot="{ handleSubmit }">
            <form @submit.prevent="handleSubmit(onShareActivityClicked)">
                <ValidationProvider rules="required" name="activityPrivacy" v-slot="{ errors, valid }" slim>
                    <b-field label="Activity Privacy"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Privacy<span class="requiredAsterisk">*</span></template>
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

                    <h3 class="title is-5">Current Members</h3>
                    <div v-if="userRoles.length > 0">
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
                    <p v-else>This activity currently has no members</p>

                </div>
                <br>

                <b-button style="float: right" @click="onShareActivityClicked"
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
    import ActivityShareConfirmation from "./ActivityShareConfirmation";
    import Api from "../Api";
    import ListItem from "./ListItem";

    export default {
        name: "ShareActivity",
        props: {
            id: {
                type: Number
            },
            activityPrivacy: {
                type: String
            }
        },
        mixins: [toastMixin],
        components: {
            ListItem,
            ValidationProvider,
            ValidationObserver
        },
        data() {
            return {
                privacy: "private",
                userRoles: [],
                originalPrivacy: "public",
                emails: {},
                newEmail: "",
                role: "",
            }
        },
        mounted() {
            this.checkAuthenticationStatus()
            this.getMembers()
        },
        methods: {
            printHelloWorld(rolesToRetain) {
                this.successToast(rolesToRetain)
            },
            onShareActivityClicked() {
                if (this.isPrivacyMoreRestrictive()) {
                    this.$buefy.modal.open({
                        parent: this,
                        events: {
                            'confirmPrivacyChange': rolesToRetain => this.printHelloWorld(rolesToRetain)
                        },
                        props: {activityId: this.id, activityPrivacy: this.privacy},
                        component: ActivityShareConfirmation,
                        trapFocus: true,
                        scroll: "clip"
                    })
                } else {
                    this.updateActivityPrivacy()
                }

            },
            updateActivityPrivacy(){
                Api.editActivityPrivacy(store.getters.getUserId, this.id, this.privacy, localStorage.getItem('authToken'))
                    .then(() => {
                        this.successToast("Activity privacy updated");
                        router.go(-1)
                    })
                    .catch(() => this.warningToast("Error updating activity privacy."));
            },
            addEmail() {
                let emailAlreadyAdded = false
                this.userRoles.forEach(user => {
                    if (user.email === this.newEmail) {
                        emailAlreadyAdded = true
                    }
                })
                if (emailAlreadyAdded) {
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
                    .catch(() => this.warningToast("Error verifying email."));

            },
            getRequestBody() {
                let payload = {
                    privacy: this.privacy,
                    members: this.userRoles
                };
                return payload;
            },
            shareActivity() {
                Api.editActivityPrivacy(store.getters.getUserId, this.id, this.getRequestBody(), localStorage.getItem('authToken'))
                    .then(() => {
                        this.successToast("Activity privacy updated")
                        router.go(-1)
                    })
                    .catch(() => this.warningToast("Error sharing activity."));
            },
            getMembers() {
                Api.getAllActivityMembers(this.id, localStorage.getItem('authToken'))
                    .then((response) => {
                        response.data.forEach(profile => {
                            if (profile.role !== "CREATOR") {
                                this.userRoles.push({email: profile.email, role: profile.role.toLowerCase()})
                            }
                        })
                    })
                    .catch(() => this.warningToast("Error getting activity members."));
            },

            deleteUser(emailToDelete) {
                this.userRoles = this.userRoles.filter(user => user.email != emailToDelete)
            },
            isPrivacyMoreRestrictive() {
                const privacyDict = {"public": 1, "friends": 2, "private": 3}
                return privacyDict[this.privacy] > privacyDict[this.originalPrivacy]
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

    .requiredAsterisk {
        color: red;
    }

</style>
