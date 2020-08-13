<template>
    <ValidationObserver v-slot="{ handleSubmit }">
        <form @submit.prevent="handleSubmit(submitParticipation)">

            <div class="container">
                <h1 class="title">Record Participation</h1>
<!--                <b-field group-multiline grouped>-->
<!--                    <b-field label="Start date" expanded>-->
<!--                        <input v-model="startDate" class="input" type="date">-->
<!--                    </b-field>-->
<!--                    <b-field label="End date" expanded>-->
<!--                        <input v-model="endDate" class="input" type="date">-->
<!--                    </b-field>-->
<!--                </b-field>-->
<!--                <b-field group-multiline grouped>-->
<!--                    <b-field label="Start time" expanded>-->
<!--                        <input v-model="startTime" class="input" type="time">-->
<!--                    </b-field>-->
<!--                    <b-field label="End time" expanded>-->
<!--                        <input v-model="endTime" class="input" type="time">-->
<!--                    </b-field>-->
<!--                </b-field>-->

                <b-field label="Start date and time">
                    <b-datetimepicker datetime-formatter="dateFormatter" v-model="startTime" placeholder="Type or select a date" editable>
                    </b-datetimepicker>
                </b-field>

                <b-field label="End date and time">
                    <b-datetimepicker v-model="endTime" placeholder="Type or select a date" editable>
                    </b-datetimepicker>
                </b-field>



                <ValidationProvider rules="required" name="Outcome" v-slot="{ errors, valid }" slim>

                <b-field label="Outcome"
                         :type="{ 'is-danger': errors[0], 'is-success': valid }"
                          :message="errors" expanded>
                    <template slot="label">Outcome <span class="requiredStar">*</span></template>
                    <b-select
                            placeholder="Outcome"
                            v-model="outcome"
                            expanded>
                        <option value="completed">Completed</option>
                        <option value="not completed">Not Completed</option>
                        <option value="disqualified">Disqualified</option>
                    </b-select>
                </b-field>
                </ValidationProvider>

                <b-field>
                    <b-field label="Details">
                        <b-input v-model="details" maxlength="200" type="textarea" placeholder="Details"></b-input>
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
    // import toastMixin from "../../mixins/toatsMixin";

    export default {
        name: "ParticipationForm",
        components: {
            ValidationProvider,
            ValidationObserver
        },
        data() {
            return {
                // startDate: null,
                // endDate: null,
                startTime: new Date(),
                endTime: new Date,
                outcome: "",
                details: ""
            }
        },
        methods: {
            submitParticipation() {
                console.log(this.startDate)
                console.log(this.outcome)
            },
            goBack() {
                router.go(-1)
            },
            validateParticipation() {

            },
            dateFormatter() {
                let options = {day:'numeric', month:'numeric', year:'numeric'}
                console.log('test')
                console.log(this.startDate.toLocaleDateString('en-GB', options))
                return this.startDate.toLocaleDateString('en-GB', options);
            }
        }
    }

</script>

<style>
    .requiredStar {
        color:red
    }

</style>