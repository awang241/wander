<template>
    <div class="container">
        <h1 class="title is-5">Record Participation</h1>
        <b-field group-multiline grouped>
            <b-field label="Start Time">
                <template slot="label">Start Time</template>
                <b-input v-model="startTime" placeholder="Start Time"></b-input>
            </b-field>
<!--            <b-field label="End Time"-->
<!--                     :type="{ 'is-danger': errors[0], 'is-success': valid }"-->
<!--                     :message="errors"-->
<!--                     expanded>-->
<!--                <template slot="label">Middle Name</template>-->
<!--                <b-input v-model="middleName" placeholder="Middle Name"></b-input>-->
<!--            </b-field>-->
            <b-field label="End Time">
                <template slot="label">End Time</template>
                <b-input v-model="endTime" placeholder="End Time"></b-input>
            </b-field>
        </b-field>

        <b-field>
            <b-field label="Outcome">
                <b-select
                        placeholder="Outcome"
                        v-model="outcome"
                        expanded>
                    <option value="female">Completed</option>
                    <option value="male">Not Completed</option>
                    <option value="non-Binary">Disqualified</option>
                </b-select>
            </b-field>
        </b-field>

        <b-fied>
            <b-field label="Comments">
                <b-input v-model="comments" maxlength="200" type="textarea" placeholder="Comments"></b-input>
            </b-field>
        </b-fied>
    </div>
</template>

<script>
    // import toastMixin from "../../mixins/toatsMixin";

</script>