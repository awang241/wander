<template>
    <ValidationObserver v-slot="{ handleSubmit }">
        <form @submit.prevent="handleSubmit(submitParticipation)">

            <div class="container">
                <h1 class="title">Record Participation</h1>
                <b-field group-multiline grouped>
                    <b-field label="Start date" expanded>
                        <input v-model="participation.startDate" class="input" type="date">
                    </b-field>
                    <b-field label="End date" expanded>
                        <input v-model="participation.endDate" class="input" type="date">
                    </b-field>
                </b-field>
                <b-field group-multiline grouped>
                    <b-field label="Start time" expanded>
                        <input v-model="participation.startTime" class="input" type="time">
                    </b-field>
                    <b-field label="End time" expanded>
                        <input v-model="participation.endTime" class="input" type="time">
                    </b-field>
                </b-field>

                <ValidationProvider rules="required" name="Outcome" v-slot="{ errors, valid }" slim>

                <b-field label="Outcome"
                         :type="{ 'is-danger': errors[0], 'is-success': valid }"
                          :message="errors" expanded>
                    <template slot="label">Outcome <span class="requiredStar">*</span></template>
                    <b-select
                            placeholder="Outcome"
                            v-model="participation.outcome"
                            expanded>
                        <option value="completed">Completed</option>
                        <option value="not completed">Not Completed</option>
                        <option value="disqualified">Disqualified</option>
                    </b-select>
                </b-field>
                </ValidationProvider>

                <b-field>
                    <b-field label="Details">
                        <b-input v-model="participation.details" maxlength="200" type="textarea" placeholder="Details"></b-input>
                    </b-field>
                </b-field>

                <div class="column">
                    <div class="is-pulled-left">
                        <b-button type="is-danger" @click="goBack">Cancel</b-button>
                    </div>
                    <div class="is-pulled-right">
                        <b-field>
                            <b-button native-type="submit" class="is-primary">Submit</b-button>
                        </b-field>
                    </div>
                    <br>
                </div>
            </div>
        </form>
    </ValidationObserver>
</template>

<script>
    import {ValidationProvider, ValidationObserver} from 'vee-validate'
    import router from "../router"
    import Api from "../Api"
    import store from "../store"
    import toastMixin from "../mixins/toastMixin"
    import dateTimeMixin from "../mixins/dateTimeMixin";

    export default {
        name: "ParticipationForm",
        components: {
            ValidationProvider,
            ValidationObserver
        },
        mixins: [toastMixin, dateTimeMixin],
        props: {
          default: function() {
              return {
                  startDate: null,
                  endDate: null,
                  startTime: "",
                  endTime: "",
                  outcome: null,
                  details: null,
              }
          }
        },
        data() {
            return {
                participation: {}
            }
        },
        computed: {
            combinedStartDate: function () {
                return this.combineDateAndTime(this.participation.startDate, this.participation.startTime)

            },
            combinedEndDate: function () {
                return this.combineDateAndTime(this.participation.endDate, this.participation.endTime)
            }
        },
        methods: {
            submitParticipation() {
                if (this.combinedStartDate !== null || this.combinedEndDate !== null){
                    if(this.validateDates()){
                        this.saveParticipation()
                    } else{
                        this.warningToast("Dates entered must be valid")
                    }
                }
                else {
                    this.saveParticipation()
                }
            },
            saveParticipation() {
                let newParticipation = {
                    "details": this.participation.details,
                    "outcome": this.participation.outcome,
                    "startTime": this.combinedStartDate,
                    "endTime": this.combinedEndDate
                }
                Api.createActivityParticipation(store.getters.getUserId, this.$route.params.id, newParticipation, localStorage.getItem('authToken'))
                    .then(() => {
                        this.successToast("Participation Added!")
                        router.go(-1)
                    })
                    .catch(() => this.warningToast("Unknown error occurred"))
            },
            goBack() {
                router.go(-1)
            },
            validateDates() {
                let isValid = true
                const startDate = Date.parse(this.combinedStartDate)
                const endDate = Date.parse(this.combinedEndDate)
                if (isNaN(startDate) || isNaN(endDate)) {
                    this.warningToast("Invalid dates entered!")
                    isValid = false
                } else if (Date.parse(this.combinedStartDate) > Date.parse(this.combinedEndDate)) {
                    this.warningToast("The end date must be after the start date")
                    isValid = false
                }
                return isValid
            }
        }
    }

</script>

<style>
    .requiredStar {
        color:red
    }

</style>